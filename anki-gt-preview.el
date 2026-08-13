;;; anki-gt-preview.el --- Card preview buffer for anki-gt  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>

;; Author: Daniel M. German <dmg@turingmachine.org>
;; Assisted-by: Claude:claude-opus-4-7
;; Maintainer: Daniel M. German <dmg@turingmachine.org>
;; Keywords: convenience, tools
;; URL: https://github.com/dmgerman/anki-gt
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Preview buffer for anki-gt.  Renders a single card's `question' and
;; `answer' HTML (as returned by AnkiConnect's `cardsInfo') into a
;; side-window using shr and libxml.
;;
;; No AnkiConnect calls happen here: the card record fetched by
;; `anki-gt-cards' already carries the HTML; the preview pane just
;; formats what's already in memory.

;;; Code:

(require 'shr)
(require 'dom)
(require 'subr-x)
(require 'url-util)
(require 'browse-url)
(require 'anki-gt)

;;;; Customization

(defgroup anki-gt-preview nil
  "Card preview options for anki-gt."
  :group 'anki-gt
  :prefix "anki-gt-preview-")

(defcustom anki-gt-preview-window-width 0.5
  "Width of the preview side-window as a fraction of the frame width."
  :type 'number
  :group 'anki-gt-preview)

(defcustom anki-gt-preview-window-side 'right
  "Side of the frame on which the preview window is displayed."
  :type '(choice (const right) (const left) (const above) (const below))
  :group 'anki-gt-preview)

(defcustom anki-gt-preview-divider-width 60
  "Width, in columns, of the divider under the preview header.
Rendering happens before the preview window is placed, so the
actual window width is not yet known; this constant is used
instead of a live measurement.  Wraps under `visual-line-mode'
in narrower windows without corrupting the layout."
  :type 'integer
  :group 'anki-gt-preview)

(defcustom anki-gt-preview-default-view 'preview
  "Which view a freshly-created preview buffer starts in.
`preview' -- shr-render the full answer HTML (as Anki would show it).
`fields'  -- show each field name as a heading with its value below.
Toggle at runtime with `anki-gt-preview-toggle-view' (bound to TAB)."
  :type '(choice (const :tag "Rendered card (preview)" preview)
                 (const :tag "Field name/value pairs (fields)" fields))
  :group 'anki-gt-preview)

(defcustom anki-gt-preview-audio-command
  (or (executable-find "afplay")
      (executable-find "mpv")
      (executable-find "ffplay"))
  "Command used to play sound files from a card preview.
Called with a single argument, the absolute path of the media
file.  Set to nil to disable audio playback.  The default probes
for `afplay' (macOS), then `mpv', then `ffplay'."
  :type '(choice (const :tag "Disabled" nil) file)
  :group 'anki-gt-preview)

(defface anki-gt-preview-header-face
  '((t :inherit font-lock-comment-face))
  "Face for the metadata header at the top of a preview buffer."
  :group 'anki-gt-preview)

(defface anki-gt-preview-divider-face
  '((t :inherit font-lock-comment-delimiter-face))
  "Face for the rule between metadata and content in a preview buffer."
  :group 'anki-gt-preview)

(defface anki-gt-preview-furigana-face
  '((t :inherit shadow :height 0.85))
  "Face applied to furigana text (the `[reading]' produced from <rt>).
Emacs cannot stack ruby text above its base, so we fall back to
inline brackets and rely on this face to keep the reading
visually distinct from the base kanji."
  :group 'anki-gt-preview)

;;;; Constants

(defconst anki-gt-preview--buffer-name "*anki-gt-preview*"
  "Name of the preview buffer.  A single instance is reused.")

;;;; Buffer-local state

(defvar-local anki-gt-preview--record nil
  "The card or note record currently rendered in this preview buffer.
Use `anki-gt-preview--record-kind' to tell which shape it has.")

(defvar-local anki-gt-preview--record-kind nil
  "Kind of `anki-gt-preview--record': `card', `note', or nil.")

(defvar-local anki-gt-preview--source-buffer nil
  "The cards buffer that opened this preview, if any.
Set by `anki-gt-preview-show'; consulted by
`anki-gt-preview-next-card' / `anki-gt-preview-previous-card'
so those commands can advance the source buffer's point and
re-render.")

(defvar-local anki-gt-preview--view-mode nil
  "Which representation this preview buffer is showing.
Either `preview' (shr-rendered answer HTML) or `fields' (name
+ value per note field).  Initialised from
`anki-gt-preview-default-view' in the mode setup and flipped by
`anki-gt-preview-toggle-view'.")

;;;; Pure helpers

(defun anki-gt-preview--header-line (record)
  "Return the one-line metadata header for RECORD.
Formatting depends on `anki-gt-preview--record-kind':

  `card' -- `Card ID  •  deck  •  model  •  Card N  •  [view]'
  `note' -- `Note ID  •  model  •  Tags: ...  •  [fields]'

Appends the current view mode in brackets so `TAB'-toggle
state is visible without checking the mode line."
  (pcase anki-gt-preview--record-kind
    ('note
     (format "Note %s  •  %s  •  Tags: %s  •  [fields]"
             (or (alist-get 'noteId record) "?")
             (or (alist-get 'modelName record) "?")
             (or (mapconcat #'identity (alist-get 'tags record) " ")
                 "")))
    (_
     (format "Card %s  •  %s  •  %s  •  Card %d  •  [%s]"
             (or (alist-get 'cardId record) "?")
             (or (alist-get 'deckName record) "?")
             (or (alist-get 'modelName record) "?")
             (1+ (or (alist-get 'ord record) 0))
             (or anki-gt-preview--view-mode 'preview)))))

(defconst anki-gt-preview--audio-url-scheme "anki-audio:"
  "URL scheme used for [sound:X] play links.")

(defconst anki-gt-preview--audio-url-regexp
  (concat "\\`" (regexp-quote anki-gt-preview--audio-url-scheme))
  "Regexp matching URLs handled by `anki-gt-preview-play-audio'.")

;;;; Media rewriting

(defun anki-gt-preview--file-url (path)
  "Return a file:// URL for the absolute file PATH.
Spaces and other reserved characters in PATH are percent-encoded."
  (url-encode-url (concat "file://" path)))

(defun anki-gt-preview--rewrite-media (html dir)
  "Return HTML with media references resolved against DIR.
Two transformations happen:
  1.  <img src=\"FILENAME\"> whose FILENAME has no URL scheme and
      is not absolute is rewritten to
      <img src=\"file:///DIR/FILENAME\"> so shr can render it.
  2.  [sound:FILENAME] is replaced by
      <a href=\"anki-audio:HEX\">[▶ FILENAME]</a>
      where HEX is the URL-encoded filename; the `anki-audio:'
      URL is dispatched to `anki-gt-preview-play-audio' via
      `browse-url'.
Absolute paths and URLs with an existing scheme are left alone."
  (with-temp-buffer
    (insert (or html ""))
    (goto-char (point-min))
    (while (re-search-forward
            "\\(<img\\b[^>]*?\\bsrc=[\"']\\)\\([^\"']+\\)\\([\"']\\)"
            nil t)
      ;; `url-encode-url' inside `anki-gt-preview--file-url' runs its
      ;; own regexes and clobbers the global match data.  That breaks
      ;; `replace-match', which reads match positions from that same
      ;; global state.  Compute the URL under `save-match-data' so the
      ;; outer match survives, and only then call `replace-match'.
      (let* ((src (match-string 2))
             (pre (match-string 1))
             (post (match-string 3))
             (url (save-match-data
                    (anki-gt-preview--file-url
                     (expand-file-name src dir)))))
        (unless (or (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" src)
                    (string-prefix-p "/" src)
                    (string-prefix-p "#" src))
          (replace-match (concat pre url post) t t))))
    (goto-char (point-min))
    (while (re-search-forward "\\[sound:\\([^]]+\\)\\]" nil t)
      (let* ((fname (match-string 1))
             ;; Save-match-data around helpers whose internal regexes
             ;; would otherwise invalidate the outer match state that
             ;; `replace-match' relies on.
             (hex (save-match-data (url-hexify-string fname)))
             (replacement (format "<a href=\"%s%s\">[▶ %s]</a>"
                                  anki-gt-preview--audio-url-scheme
                                  hex fname)))
        (replace-match replacement t t)))
    (buffer-string)))

;;;; Audio playback

(defun anki-gt-preview--resolve-media (fname)
  "Return the absolute path of FNAME under `anki-gt-media-dir', or nil.
First tries FNAME as-given; if that does not exist, retries with
just its basename (some Yomichan-style imports store paths like
`sub/foo.mp3' in the field while the file on disk is flat)."
  (let* ((dir (anki-gt-media-dir))
         (direct (expand-file-name fname dir)))
    (cond
     ((file-exists-p direct) direct)
     ((let ((flat (expand-file-name (file-name-nondirectory fname) dir)))
        (and (file-exists-p flat) flat))))))

(defun anki-gt-preview-play-audio (url &rest _args)
  "Play the audio file referenced by URL.
URL is expected to have the `anki-audio:' scheme, produced by
`anki-gt-preview--rewrite-media'.  The path is URL-decoded and
resolved against `anki-gt-media-dir' via
`anki-gt-preview--resolve-media' (which falls back to the
basename when the exact path is missing).  Runs
`anki-gt-preview-audio-command' as an asynchronous subprocess;
errors with a clear message if that variable is nil or the file
cannot be resolved."
  (unless anki-gt-preview-audio-command
    (user-error "`anki-gt-preview-audio-command' is nil; audio disabled"))
  (let* ((encoded (substring url
                             (length anki-gt-preview--audio-url-scheme)))
         (fname (url-unhex-string encoded))
         (path (anki-gt-preview--resolve-media fname)))
    (unless path
      (user-error "Media file not found for [sound:%s]" fname))
    (start-process "anki-gt-audio" nil
                   anki-gt-preview-audio-command path)
    (message "Playing %s" fname)))

(defun anki-gt-preview--register-audio-handler ()
  "Register `anki-gt-preview-play-audio' as a `browse-url' handler.
Uses `browse-url-handlers' (Emacs 28+) when available, and falls
back to the alist form of `browse-url-browser-function' on
earlier versions."
  (let ((entry (cons anki-gt-preview--audio-url-regexp
                     #'anki-gt-preview-play-audio)))
    (cond
     ((boundp 'browse-url-handlers)
      (add-to-list 'browse-url-handlers entry))
     ((and (boundp 'browse-url-browser-function)
           (listp browse-url-browser-function))
      (add-to-list 'browse-url-browser-function entry)))))

(anki-gt-preview--register-audio-handler)

(defun anki-gt-preview--dom-text (node)
  "Return the concatenated text of NODE and all its descendants.
Compat shim for `dom-texts' (obsolete since Emacs 31.1) and
`dom-inner-text' (not present in older Emacsen)."
  (cond
   ((stringp node) node)
   ((listp node)
    (mapconcat #'anki-gt-preview--dom-text (dom-children node) ""))
   (t "")))

(defun anki-gt-preview--render-ruby (dom)
  "Render a <ruby> DOM node as `base[reading]'.
Emitted between the base text and the bracketed reading, the
reading itself is fontified with `anki-gt-preview-furigana-face'
so it stays visually distinct.  Handles the common shapes of
Anki-generated ruby markup:
  <ruby>base<rt>reading</rt></ruby>
  <ruby><rb>base</rb><rt>reading</rt></ruby>
  <ruby>base1<rt>r1</rt>base2<rt>r2</rt></ruby>
<rp> children (parenthesis fallback) are dropped since our
brackets already serve that role."
  (dolist (child (dom-children dom))
    (cond
     ((stringp child)
      (insert child))
     ((not (listp child))
      nil)
     ((eq (dom-tag child) 'rt)
      ;; With ruby off, drop the reading entirely; the base text was
      ;; already emitted by other children of this <ruby> node.
      (when anki-gt-show-ruby
        (let ((start (point)))
          (insert "[")
          (insert (anki-gt-preview--dom-text child))
          (insert "]")
          (put-text-property start (point) 'face
                             'anki-gt-preview-furigana-face))))
     ((eq (dom-tag child) 'rb)
      ;; Bypass shr-generic here to avoid its fill-line logic breaking
      ;; between CJK characters.  Any inline markup nested inside <rb>
      ;; (very rare in practice) is flattened to its text content.
      (insert (anki-gt-preview--dom-text child)))
     ((eq (dom-tag child) 'rp)
      nil)
     (t
      (shr-descend child)))))

(defun anki-gt-preview--html-to-dom (html)
  "Parse the HTML string HTML into a libxml DOM.
Returns nil when HTML is empty or libxml is unavailable."
  (when (and (stringp html)
             (not (string-empty-p html))
             (fboundp 'libxml-parse-html-region))
    (with-temp-buffer
      (insert html)
      (libxml-parse-html-region (point-min) (point-max)))))

;;;; Minimal CSS support
;;
;; shr ignores class-scoped stylesheets and inline `style' attributes,
;; so per-field font sizes in Anki templates never come through.  We
;; parse the card's `css' field ourselves and apply the small subset
;; of properties that map cleanly onto Emacs face attributes.
;;
;; Supported:
;;   selectors  -- `.class', `#id', `tag', `tag.class'
;;   properties -- font-size, font-weight, color, background-color
;;
;; Complex CSS (descendant combinators, pseudo-classes, media queries)
;; is silently ignored.  Rules whose declarations don't touch a
;; supported property are dropped up front so the per-node lookup
;; during render stays cheap.

(defvar-local anki-gt-preview--css-rules nil
  "Buffer-local parsed CSS rules for the currently rendered card.
Each rule is (:selectors LIST-OF-STRINGS :props ALIST-OF-CSS-PROPS).")

(defconst anki-gt-preview--css-supported-props
  '("font-size" "font-weight" "color" "background-color")
  "CSS property names that translate onto Emacs face attributes.")

(defun anki-gt-preview--strip-css-comments (css)
  "Return CSS with `/* ... */' comments removed."
  (replace-regexp-in-string
   "/\\*[^*]*\\*+\\(?:[^/*][^*]*\\*+\\)*/" "" css))

(defun anki-gt-preview--parse-css-declarations (body)
  "Parse BODY (the `{...}' contents of one rule) into an alist.
Only declarations whose property is in
`anki-gt-preview--css-supported-props' are returned."
  (let ((result nil))
    (dolist (decl (split-string body ";" t "[ \t\n]+"))
      (when (string-match "\\([-a-zA-Z]+\\)\\s-*:\\s-*\\(.+\\)" decl)
        (let ((name (match-string 1 decl))
              (val  (string-trim (match-string 2 decl))))
          (when (member name anki-gt-preview--css-supported-props)
            (push (cons name val) result)))))
    (nreverse result)))

(defun anki-gt-preview--parse-css (css)
  "Parse CSS into a list of rules.
Each rule is (:selectors LIST :props ALIST); rules with no
supported declarations are dropped.  Ignores at-rules and
anything with nested braces (e.g. `@media')."
  (when (stringp css)
    (let ((stripped (anki-gt-preview--strip-css-comments css))
          (result nil)
          (pos 0))
      (while (string-match "\\([^{}]+\\){\\([^{}]*\\)}" stripped pos)
        ;; Capture positions and captures BEFORE calling
        ;; `anki-gt-preview--parse-css-declarations', which runs its
        ;; own regex and clobbers the global match data.  Without
        ;; this, `(match-end 0)' below would read the inner match's
        ;; end, `pos' would fail to advance past this rule, and the
        ;; loop would re-match the same rule forever.
        ;; Read every capture from the current match BEFORE calling
        ;; helpers like `string-trim' or `anki-gt-preview--parse-...',
        ;; which run their own regexes and clobber the global match
        ;; data.  Missing this cost me two rounds of debugging today
        ;; -- the same hazard also bit `--rewrite-media' above.
        (let* ((next-pos (match-end 0))
               (sel-raw  (match-string 1 stripped))
               (body     (match-string 2 stripped))
               (sel-trim (string-trim sel-raw))
               (props    (anki-gt-preview--parse-css-declarations body)))
          (unless (string-prefix-p "@" sel-trim)
            (let ((sels (split-string sel-trim "," t "[ \t\n]+")))
              (when props
                (push (list :selectors sels :props props) result))))
          (setq pos next-pos)))
      (nreverse result))))

(defun anki-gt-preview--selector-matches-p (selector dom)
  "Return non-nil if the CSS SELECTOR matches DOM node.
Supports `.class', `#id', `tag', and `tag.class' selectors only."
  (let* ((tag (dom-tag dom))
         (classes (split-string (or (dom-attr dom 'class) "") "[ \t]+" t))
         (id (dom-attr dom 'id)))
    (cond
     ((string-prefix-p "." selector)
      (and (member (substring selector 1) classes) t))
     ((string-prefix-p "#" selector)
      (equal (substring selector 1) id))
     ((string-match "\\`\\([a-zA-Z]+\\)\\.\\([-_a-zA-Z0-9]+\\)\\'" selector)
      (and (equal (match-string 1 selector) (symbol-name tag))
           (member (match-string 2 selector) classes)
           t))
     ((string-match "\\`[a-zA-Z]+\\'" selector)
      (equal selector (symbol-name tag))))))

(defun anki-gt-preview--css-font-size-height (val)
  "Convert a CSS font-size VAL to an Emacs face `:height' value.
`%'/`em'/`rem' become float multipliers; `px' is approximated
against a 16-px baseline; `pt' becomes an absolute value in
1/10 pt (Emacs' `:height' scale).  Returns nil on unparseable
input.  Absolute keywords (`larger', `smaller') are also nil."
  (cond
   ((string-match "\\`\\([0-9.]+\\)%\\'" val)
    (/ (string-to-number (match-string 1 val)) 100.0))
   ((string-match "\\`\\([0-9.]+\\)r?em\\'" val)
    (float (string-to-number (match-string 1 val))))
   ((string-match "\\`\\([0-9.]+\\)px\\'" val)
    (/ (float (string-to-number (match-string 1 val))) 16.0))
   ((string-match "\\`\\([0-9.]+\\)pt\\'" val)
    (truncate (* (string-to-number (match-string 1 val)) 10)))))

(defun anki-gt-preview--props-to-face (props)
  "Convert an alist of CSS PROPS to an Emacs face-spec plist.
Returns nil when no property produced an attribute."
  (let (face)
    (dolist (p props)
      (pcase (car p)
        ("font-size"
         (let ((h (anki-gt-preview--css-font-size-height (cdr p))))
           (when h (setq face (plist-put face :height h)))))
        ("color"
         (setq face (plist-put face :foreground (cdr p))))
        ("background-color"
         (setq face (plist-put face :background (cdr p))))
        ("font-weight"
         (when (or (equal (cdr p) "bold")
                   (let ((n (string-to-number (cdr p))))
                     (and (> n 0) (>= n 600))))
           (setq face (plist-put face :weight 'bold))))))
    face))

(defun anki-gt-preview--face-for-node (dom)
  "Return an Emacs face spec for DOM based on `anki-gt-preview--css-rules'.
Concatenates props from every matching rule in source order,
so later rules override earlier ones on the same key."
  (let (props)
    (dolist (rule anki-gt-preview--css-rules)
      (dolist (sel (plist-get rule :selectors))
        (when (anki-gt-preview--selector-matches-p sel dom)
          (setq props (append props (plist-get rule :props))))))
    (anki-gt-preview--props-to-face props)))

(defun anki-gt-preview--render-styled-container (dom)
  "Render DOM's children via shr, then apply the CSS-derived face.
Serves as a shr handler for `div' and `span'.  Falls back to
plain `shr-generic' behavior when no CSS rule matches DOM, so
styleless containers cost nothing."
  (let ((face (anki-gt-preview--face-for-node dom))
        (start (point)))
    (shr-generic dom)
    (when face
      (add-face-text-property start (point) face))))

;;;; Field-level audio extraction

(defun anki-gt-preview--sound-refs-in-string (str)
  "Return the list of [sound:X] filenames found in STR, in order.
Duplicates are preserved so callers can decide whether to
deduplicate.  Returns nil when STR is nil or contains no matches."
  (when (stringp str)
    (let ((refs nil)
          (pos 0))
      (while (string-match "\\[sound:\\([^]]+\\)\\]" str pos)
        (push (match-string 1 str) refs)
        (setq pos (match-end 0)))
      (nreverse refs))))

(defun anki-gt-preview--card-audio-refs (card)
  "Return an ordered, de-duplicated list of audio references in CARD.
Each element is a cons (FIELD-NAME . FILENAME), where FIELD-NAME
is the source field's symbol (e.g. `wordAudio') and FILENAME is
the media filename from a [sound:...] marker in that field.
Duplicates by FILENAME are dropped, keeping the first FIELD-NAME
seen.  Scans every field value (not the rendered HTML) so refs
survive template preprocessing to [anki:play:...] placeholders."
  (let ((seen (make-hash-table :test 'equal))
        (result nil))
    (dolist (pair (alist-get 'fields card))
      (dolist (fname (anki-gt-preview--sound-refs-in-string
                      (alist-get 'value (cdr pair))))
        (unless (gethash fname seen)
          (puthash fname t seen)
          (push (cons (car pair) fname) result))))
    (nreverse result)))

(defvar anki-gt-preview--audio-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")       #'anki-gt-preview--play-button-at-point)
    (define-key map [mouse-2]         #'anki-gt-preview--play-button-at-mouse)
    (define-key map [follow-link]     'mouse-face)
    map)
  "Keymap active on an audio play button in the preview buffer.")

(defface anki-gt-preview-audio-button-face
  '((t :inherit link))
  "Face for clickable audio play buttons in the preview."
  :group 'anki-gt-preview)

(defface anki-gt-preview-audio-missing-face
  '((t :inherit shadow :strike-through t))
  "Face for [✗ name] labels when a referenced audio file is missing."
  :group 'anki-gt-preview)

(defface anki-gt-preview-field-name-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for field-name headings in the fields view."
  :group 'anki-gt-preview)

(defun anki-gt-preview--play-button-at-point ()
  "Play the audio file associated with the button at point."
  (interactive)
  (let ((url (get-text-property (point) 'anki-gt-audio-url)))
    (if url
        (anki-gt-preview-play-audio url)
      (user-error "No audio button at point"))))

(defun anki-gt-preview--play-button-at-mouse (event)
  "Play the audio file associated with the button at the mouse EVENT."
  (interactive "e")
  (let* ((pos (posn-point (event-end event)))
         (url (and pos (get-text-property pos 'anki-gt-audio-url))))
    (if url
        (anki-gt-preview-play-audio url)
      (user-error "No audio button under mouse"))))

(defun anki-gt-preview--insert-audio-section (card)
  "Insert an Audio section for CARD, if any [sound:X] refs are present.
Emits one line per reference, formatted as
  FIELD-NAME   [▶ FILENAME]
with FIELD-NAME padded so the buttons align.  Refs whose file
cannot be resolved on disk are rendered as [✗ FILENAME] in
`anki-gt-preview-audio-missing-face' and are not clickable, so
you see at a glance which audio is actually playable.  Silent
no-op when the card has no audio."
  (let ((refs (anki-gt-preview--card-audio-refs card)))
    (when refs
      (insert (propertize "Audio\n" 'face 'anki-gt-preview-header-face))
      (let ((label-width
             (apply #'max (mapcar (lambda (r)
                                    (length (symbol-name (car r))))
                                  refs))))
        (dolist (ref refs)
          (let* ((field (symbol-name (car ref)))
                 (fname (cdr ref))
                 (path (ignore-errors
                         (anki-gt-preview--resolve-media fname))))
            (insert "  ")
            (insert (format (format "%%-%ds  " label-width) field))
            (if path
                (let ((url   (concat anki-gt-preview--audio-url-scheme
                                     (save-match-data
                                       (url-hexify-string fname))))
                      (start (point)))
                  (insert (format "[▶ %s]" fname))
                  (add-text-properties
                   start (point)
                   `(face anki-gt-preview-audio-button-face
                     mouse-face highlight
                     help-echo ,(format "mouse-2/RET: play %s" fname)
                     anki-gt-audio-url ,url
                     keymap ,anki-gt-preview--audio-button-map)))
              (insert (propertize
                       (format "[✗ %s]" fname)
                       'face 'anki-gt-preview-audio-missing-face
                       'help-echo (format "Missing in collection.media: %s"
                                          fname))))
            (insert "\n"))))
      (insert "\n"))))

;;;; Rendering

(defun anki-gt-preview--prepare-html (html)
  "Return HTML with media rewritten and literal furigana stripped when off."
  (let* ((dir (ignore-errors (anki-gt-media-dir)))
         (out (if (and html dir)
                  (anki-gt-preview--rewrite-media html dir)
                html)))
    (if anki-gt-show-ruby out (anki-gt-strip-literal-furigana out))))

(defun anki-gt-preview--shr-render-html (html)
  "Insert HTML at point, rendered via shr with our custom handlers.
Applies the ruby handler always, and the CSS-driven div/span
handlers only when `anki-gt-preview--css-rules' is non-nil so
the unstyled case stays on shr's fast path."
  (let ((dom (anki-gt-preview--html-to-dom html))
        (shr-external-rendering-functions
         (append
          (when anki-gt-preview--css-rules
            '((div  . anki-gt-preview--render-styled-container)
              (span . anki-gt-preview--render-styled-container)))
          (list (cons 'ruby #'anki-gt-preview--render-ruby))
          shr-external-rendering-functions)))
    (cond
     (dom (shr-insert-document dom))
     ;; Fallback: strip tags manually so at least text is visible.
     (html (insert (replace-regexp-in-string "<[^>]+>" "" html)))
     (t (insert "[no content]")))))

(defun anki-gt-preview--insert-answer (card)
  "Insert CARD's answer HTML shr-rendered into the current buffer."
  (anki-gt-preview--shr-render-html
   (anki-gt-preview--prepare-html (alist-get 'answer card))))

(defcustom anki-gt-preview-fields-name-width 'auto
  "Width in columns of the field-name column in the fields view.
`auto' (the default) picks the max field-name length capped by
`anki-gt-preview-fields-name-width-max'.  An integer overrides
that computation."
  :type '(choice (const :tag "Auto (fit longest name)" auto)
                 (integer :tag "Fixed width"))
  :group 'anki-gt-preview)

(defcustom anki-gt-preview-fields-name-width-max 24
  "Cap for `auto' width in the field-name column.
Field names longer than this are truncated with a trailing `…'."
  :type 'integer
  :group 'anki-gt-preview)

(defun anki-gt-preview--fields-name-width (fields)
  "Return the column width to use for the field-name column in FIELDS."
  (pcase anki-gt-preview-fields-name-width
    ('auto (min anki-gt-preview-fields-name-width-max
                (apply #'max 4
                       (mapcar (lambda (p) (length (symbol-name (car p))))
                               fields))))
    ((and (pred integerp) n) n)
    (_ anki-gt-preview-fields-name-width-max)))

(defun anki-gt-preview--insert-fields (card)
  "Insert CARD's fields as two columns: name + shr-rendered value.
Fields are emitted in `order' sequence.  Value cells that wrap
\(soft wrap or explicit `<br>' / `<p>' newlines) continue at the
value column via `line-prefix' / `wrap-prefix' text properties,
so long values stay readable without breaking the alignment."
  (let* ((fields (alist-get 'fields card))
         (sorted (sort (copy-sequence fields)
                       (lambda (a b)
                         (< (or (alist-get 'order (cdr a)) 0)
                            (or (alist-get 'order (cdr b)) 0)))))
         (name-w (anki-gt-preview--fields-name-width sorted))
         (gutter "  ")
         (indent (make-string (+ name-w (length gutter)) ?\ )))
    (dolist (pair sorted)
      (let* ((raw-name (symbol-name (car pair)))
             (name-cell (truncate-string-to-width
                         raw-name name-w nil ?\  "…"))
             (value (alist-get 'value (cdr pair))))
        (insert (propertize (concat name-cell gutter)
                            'face 'anki-gt-preview-field-name-face))
        ;; A marker with insertion-type nil stays anchored to the first
        ;; char of the value region even when shr inserts (and we
        ;; later delete) around it.
        (let ((value-start (point-marker)))
          (set-marker-insertion-type value-start nil)
          (if (and value (not (string-empty-p value)))
              (anki-gt-preview--shr-render-html
               (anki-gt-preview--prepare-html value))
            (insert (propertize "(empty)" 'face 'shadow)))
          ;; Block-level shr renderers (`<p>', `<div>') pad the value
          ;; with leading newlines AND leading indent, and a trailing
          ;; newline afterwards.  Strip all of that so every field is
          ;; exactly one row (plus soft wraps) and the value column
          ;; aligns flush against the gutter regardless of the value's
          ;; HTML shape.  Legitimate leading whitespace inside a
          ;; preserved-whitespace value is a niche loss.
          (save-excursion
            (goto-char value-start)
            (while (and (< (point) (point-max))
                        (memq (char-after) '(?\n ?\s ?\t)))
              (delete-char 1)))
          (while (and (> (point) (marker-position value-start))
                      (eq (char-before) ?\n))
            (delete-char -1))
          ;; Propagate the value column's indent to every wrapped or
          ;; newlined continuation, so the value stays a rectangle.
          (add-text-properties value-start (point)
                               `(line-prefix ,indent wrap-prefix ,indent))
          (set-marker value-start nil))
        (insert "\n")))))

(defun anki-gt-preview--render (record)
  "Replace the current buffer contents with a rendering of RECORD.
RECORD is a card or note record; the choice of view is driven
by `anki-gt-preview--record-kind':

  `card' -- honours `anki-gt-preview--view-mode' (preview |
            fields).  `preview' shr-renders the full answer
            HTML (Anki-like); `fields' emits the two-column
            field table.
  `note' -- always the fields table.  Notes have no rendered
            answer to show.

Both kinds share the metadata header, divider, and audio section."
  (let ((inhibit-read-only t))
    (setq anki-gt-preview--css-rules
          (anki-gt-preview--parse-css (alist-get 'css record)))
    (erase-buffer)
    (insert (propertize (anki-gt-preview--header-line record)
                        'face 'anki-gt-preview-header-face))
    (insert "\n")
    (insert (propertize (make-string anki-gt-preview-divider-width ?─)
                        'face 'anki-gt-preview-divider-face))
    (insert "\n\n")
    (anki-gt-preview--insert-audio-section record)
    (cond
     ((eq anki-gt-preview--record-kind 'note)
      (anki-gt-preview--insert-fields record))
     ((eq anki-gt-preview--view-mode 'fields)
      (anki-gt-preview--insert-fields record))
     (t
      (anki-gt-preview--insert-answer record)))
    (goto-char (point-min))
    (setq anki-gt-preview--record record)))

;;;; Interactive commands

(defun anki-gt-preview-refresh ()
  "Re-render the currently displayed record.
Useful after changing shr configuration or a font."
  (interactive)
  (unless anki-gt-preview--record
    (user-error "No card or note in this preview buffer"))
  (anki-gt-preview--render anki-gt-preview--record))

(defun anki-gt-preview-quit ()
  "Close the preview side-window without killing the buffer."
  (interactive)
  (quit-window))

(defun anki-gt-preview-open-in-anki ()
  "Open the currently displayed record in Anki and raise the window.
For a card, filters the Card Browser to that card (`cid:').
For a note, opens the note's edit dialog (`guiEditNote')."
  (interactive)
  (unless anki-gt-preview--record
    (user-error "No card or note in this preview buffer"))
  (pcase anki-gt-preview--record-kind
    ('note
     (anki-gt-request "guiEditNote"
                      `((note . ,(alist-get 'noteId anki-gt-preview--record))))
     (anki-gt-raise-anki))
    (_
     (anki-gt-open-card-in-anki anki-gt-preview--record))))

(defun anki-gt-preview-toggle-view ()
  "Toggle the preview between rendered-card and fields views.
Errors for notes -- note view is fields-only, since a note has
no single rendered answer to show.  For cards, flips
`anki-gt-preview--view-mode' between `preview' and `fields' and
re-renders."
  (interactive)
  (unless anki-gt-preview--record
    (user-error "No card or note in this preview buffer"))
  (when (eq anki-gt-preview--record-kind 'note)
    (user-error "Note view shows fields only (no rendered card)"))
  (setq anki-gt-preview--view-mode
        (if (eq anki-gt-preview--view-mode 'fields) 'preview 'fields))
  (anki-gt-preview--render anki-gt-preview--record))

;;;; Mode definition

(defvar anki-gt-preview-mode-map (make-sparse-keymap)
  "Keymap for `anki-gt-preview-mode'.")

;; Bindings are re-applied every time this file loads so that adding a
;; new key doesn't require restarting Emacs.  A bare `defvar' would
;; short-circuit on reload and leave the running keymap out of date.
(let ((map anki-gt-preview-mode-map))
  (define-key map (kbd "q") #'anki-gt-preview-quit)
  (define-key map (kbd "g") #'anki-gt-preview-refresh)
  (define-key map (kbd "n") #'anki-gt-preview-next-card)
  (define-key map (kbd "p") #'anki-gt-preview-previous-card)
  (define-key map (kbd "r")   #'anki-gt-toggle-ruby)
  (define-key map (kbd "a")   #'anki-gt-preview-open-in-anki)
  (define-key map (kbd "TAB") #'anki-gt-preview-toggle-view))

(define-derived-mode anki-gt-preview-mode special-mode "Anki-Preview"
  "Major mode for anki-gt card preview buffers.
\\{anki-gt-preview-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines nil)
  (setq-local anki-gt-preview--view-mode anki-gt-preview-default-view)
  (visual-line-mode 1))

;;;; Public entry points

(defun anki-gt-preview--show-record (record kind &optional source-buffer)
  "Display RECORD (a card or note) in the shared preview buffer.
KIND is `card' or `note'.  When SOURCE-BUFFER (a list buffer) is
supplied it is remembered in `anki-gt-preview--source-buffer'
so `anki-gt-preview-next-card' and its sibling can walk the
underlying list.  The buffer is placed in a side-window per
`anki-gt-preview-window-side' and `anki-gt-preview-window-width'.
Returns the preview buffer."
  (let ((buffer (get-buffer-create anki-gt-preview--buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'anki-gt-preview-mode)
        (anki-gt-preview-mode))
      (setq anki-gt-preview--record-kind kind)
      (when source-buffer
        (setq anki-gt-preview--source-buffer source-buffer))
      (anki-gt-preview--render record))
    (display-buffer
     buffer
     `((display-buffer-in-side-window)
       (side . ,anki-gt-preview-window-side)
       (window-width . ,anki-gt-preview-window-width)))
    buffer))

(defun anki-gt-preview-show (card &optional source-buffer)
  "Display CARD in the shared preview buffer.
SOURCE-BUFFER, when supplied, is remembered for n/p navigation.
Kept for backward compatibility -- prefer
`anki-gt-preview-show-card'."
  (anki-gt-preview--show-record card 'card source-buffer))

(defun anki-gt-preview-show-card (card &optional source-buffer)
  "Display CARD in the shared preview buffer.
SOURCE-BUFFER, when supplied, is remembered for n/p navigation.
See `anki-gt-preview--show-record' for full semantics."
  (anki-gt-preview--show-record card 'card source-buffer))

(defun anki-gt-preview-show-note (note &optional source-buffer)
  "Display NOTE in the shared preview buffer, always in fields view.
SOURCE-BUFFER, when supplied, is remembered for n/p navigation.
See `anki-gt-preview--show-record' for full semantics."
  (anki-gt-preview--show-record note 'note source-buffer))

(defun anki-gt-preview--navigate-source (n)
  "Move the source cards buffer's point by N and re-render preview.
Signals an error if there is no linked source buffer or if the
move would go past either end of the list."
  (unless (buffer-live-p anki-gt-preview--source-buffer)
    (user-error "No linked cards buffer"))
  (let ((source anki-gt-preview--source-buffer))
    (with-current-buffer source
      (let ((orig (point)))
        (forward-line n)
        (let ((card (tabulated-list-get-id)))
          (unless card
            (goto-char orig)
            (user-error (if (> n 0) "End of list" "Start of list")))
          (let ((win (get-buffer-window source)))
            (when win (set-window-point win (point))))
          (anki-gt-preview-show card source))))))

(defun anki-gt-preview-next-card ()
  "Advance the source cards buffer to the next card and re-render preview."
  (interactive)
  (anki-gt-preview--navigate-source 1))

(defun anki-gt-preview-previous-card ()
  "Retreat the source cards buffer to the previous card and re-render preview."
  (interactive)
  (anki-gt-preview--navigate-source -1))

(defun anki-gt-preview-close ()
  "Delete the preview side-window if it is visible.
Does nothing if the preview buffer is not currently displayed."
  (interactive)
  (let* ((buffer (get-buffer anki-gt-preview--buffer-name))
         (window (and buffer (get-buffer-window buffer))))
    (when window
      (delete-window window))))

(defun anki-gt-preview-visible-p ()
  "Return non-nil when the preview buffer is displayed in some window."
  (let ((buffer (get-buffer anki-gt-preview--buffer-name)))
    (and buffer (get-buffer-window buffer))))

(provide 'anki-gt-preview)

;;; anki-gt-preview.el ends here
