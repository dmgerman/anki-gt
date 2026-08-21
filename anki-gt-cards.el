;;; anki-gt-cards.el --- Card-list buffer for anki-gt  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>

;; Author: Daniel M. German <dmg@turingmachine.org>
;; Assisted-by: Claude:claude-opus-4-7
;; Maintainer: Daniel M. German <dmg@turingmachine.org>
;; Keywords: convenience, tools
;; URL: https://github.com/dmgerman/anki-gt
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Card-list buffer for anki-gt.  Displays cards matching an
;; AnkiConnect search query in a tabulated view, one row per card.
;;
;; Entry point: `anki-gt-cards-open QUERY &optional LABEL' -- called
;; from `anki-gt-main-open-at-point' when the user presses RET on a
;; deck line.  The query buffer, faceted narrowing, and preview pane
;; are follow-up slices.
;;
;; Fetch strategy: one `findCards' call for the full ID list, then
;; `cardsInfo' for the first `anki-gt-cards-limit' IDs.  `M' loads the
;; next batch.  This keeps the initial paint responsive on decks with
;; many thousands of cards without truncating the underlying result set.

;;; Code:

(require 'anki-gt)
(require 'tabulated-list)
(require 'seq)
(require 'cl-lib)
(require 'subr-x)

;;;; Customization

(defcustom anki-gt-cards-limit 500
  "Maximum number of cards fetched from `cardsInfo' per batch.
The full `findCards' ID list is always retrieved; this option
only caps how many cards are turned into display rows in a single
fetch.  Use `\\<anki-gt-cards-mode-map>\\[anki-gt-cards-load-more]' to load the next batch."
  :type 'integer
  :group 'anki-gt)

(defcustom anki-gt-cards-field-width 30
  "Display width, in columns, of the note-field column.
The column shows each note's model-designated display field (its
sort field), matching Anki's own Card Browser.  Kept modest so
the numeric trailing columns stay visible in narrower windows;
increase if your field content is long and your window is wide."
  :type 'integer
  :group 'anki-gt)

(defcustom anki-gt-cards-deck-width 28
  "Display width, in columns, of the Deck column.
Full deck paths are truncated via `anki-gt-truncate-middle',
which preserves the leaf deck name when it fits."
  :type 'integer
  :group 'anki-gt)

(defcustom anki-gt-cards-preview-auto t
  "When non-nil, moving point in a cards buffer refreshes the preview.
Automatic refresh only fires while the preview window is
already visible; it never spontaneously opens the preview."
  :type 'boolean
  :group 'anki-gt)

;;;; Constants

(defconst anki-gt-cards--state-labels
  '((-3 . "Bury")
    (-2 . "Bury")
    (-1 . "Susp")
    (0  . "New")
    (1  . "Learn")
    (2  . "Due")
    (3  . "Learn")
    (4  . "Prev"))
  "Mapping from a card's `queue' field to a short display label.
Values follow Anki's internal queue codes: -3 buried by
scheduler, -2 buried by user, -1 suspended, 0 new, 1 learning,
2 review, 3 day-learning, 4 preview.")

;;;; Buffer-local state

(defvar-local anki-gt-cards--query nil
  "The AnkiConnect search query displayed in this buffer.")

(defvar-local anki-gt-cards--label nil
  "Human-readable label for this query (e.g. a deck name).")

(defvar-local anki-gt-cards--all-ids nil
  "Full list of card IDs matching `anki-gt-cards--query'.")

(defvar-local anki-gt-cards--fetched 0
  "Count of cards already fetched via `cardsInfo' and displayed.")

(defvar-local anki-gt-cards--last-previewed nil
  "The card record most recently sent to the preview pane.
Used to skip redundant re-renders when point movement stays on
the same row.")

(defvar-local anki-gt-cards--query-stack nil
  "Stack of previous queries in this buffer.
Pushed by `anki-gt-cards-narrow', popped by `anki-gt-cards-widen'.
Cleared by `anki-gt-cards-search'.")

(defvar anki-gt-cards--templates-cache (make-hash-table :test 'equal)
  "Cache of MODEL-NAME → list of template names indexed by `ord'.
Populated lazily by `anki-gt-cards--prefetch-templates' via
`modelTemplates'.  Global so multiple cards buffers share the
lookup; invalidate with `anki-gt-cards-templates-cache-invalidate'
after editing a model in Anki.")

(defun anki-gt-cards-templates-cache-invalidate ()
  "Discard the cached model → template-names table.
Use after editing a model's card templates in Anki so the cards
buffer picks up the new template names."
  (interactive)
  (clrhash anki-gt-cards--templates-cache)
  (when (called-interactively-p 'interactive)
    (message "anki-gt template-names cache cleared")))

(defvar anki-gt-cards--query-history nil
  "Minibuffer history for AnkiConnect search prompts.
Shared across all cards buffers and the main-buffer `s' command.")

;;;; Card record accessors (pure)

;; The ruby-transform, HTML-strip, and field-by-order helpers used to
;; live here; they were extracted to anki-gt.el so anki-gt-notes.el
;; can reuse them without depending on anki-gt-cards.el.  These
;; wrappers preserve the existing private names so nothing else in
;; this file has to move.

(defalias 'anki-gt-cards--transform-ruby #'anki-gt-transform-ruby)
(defalias 'anki-gt-cards--strip-html     #'anki-gt-strip-html)
(defalias 'anki-gt-cards--field-at-order #'anki-gt-field-at-order)

(defun anki-gt-cards--display-field (card)
  "Return the CARD's model-designated display field value.
This mirrors Anki's own Card Browser, which shows each model's
sort field.  Uses the `fieldOrder' value carried on the card
record (populated by AnkiConnect from the model's `sortf'
setting) when present, and falls back to the field with order 0."
  (anki-gt-field-at-order
   card
   (or (alist-get 'fieldOrder card) 0)))

(defun anki-gt-cards--state-label (card)
  "Return a short label describing CARD's scheduling state."
  (or (alist-get (alist-get 'queue card) anki-gt-cards--state-labels)
      "?"))

(defun anki-gt-cards--format-interval (card)
  "Return CARD's interval as a short display string.
Positive values are days.  Negative values are seconds (learning
step) and are rendered with an `s' suffix."
  (let ((n (alist-get 'interval card)))
    (cond
     ((null n) "")
     ((> n 0) (format "%dd" n))
     ((< n 0) (format "%ds" (- n)))
     (t "0"))))

(defun anki-gt-cards--prefetch-templates (cards)
  "Populate `anki-gt-cards--templates-cache' for every model in CARDS.
Issues one `modelTemplates' call per distinct model that is not
already cached.  Safe to call repeatedly; already-cached models
are skipped.  Silently swallows individual model-lookup failures
so a single broken model does not abort the batch."
  (let ((models (delete-dups
                 (delq nil
                       (mapcar (lambda (c) (alist-get 'modelName c))
                               cards)))))
    (dolist (model models)
      (unless (gethash model anki-gt-cards--templates-cache)
        (condition-case _err
            (let* ((response (anki-gt-request "modelTemplates"
                                              `((modelName . ,model))))
                   (names (mapcar (lambda (pair) (symbol-name (car pair)))
                                  response)))
              (puthash model names anki-gt-cards--templates-cache))
          (error nil))))))

(defun anki-gt-cards--template-label (card)
  "Return the human name of CARD's template.
Reads from `anki-gt-cards--templates-cache' (populated by
`anki-gt-cards--prefetch-templates' at fetch time).  Falls back
to `Card N' when the model has no cached template list -- e.g.
for cards fetched before the cache was populated."
  (let* ((model (alist-get 'modelName card))
         (ord (or (alist-get 'ord card) 0))
         (names (and model (gethash model anki-gt-cards--templates-cache))))
    (or (and names (nth ord names))
        (format "Card %d" (1+ ord)))))

;;;; Entry construction

(defun anki-gt-cards--deck-cell (card)
  "Return the display value for CARD's Deck column.
Full deck path from `deckName', middle-truncated to
`anki-gt-cards-deck-width' via `anki-gt-truncate-middle'."
  (anki-gt-truncate-middle (or (alist-get 'deckName card) "")
                           anki-gt-cards-deck-width))

(defun anki-gt-cards--entry (card)
  "Convert a CARD alist to a `tabulated-list-mode' entry.
The row's ID is the CARD alist itself, so commands can retrieve
it via `tabulated-list-get-id' without a side hash table.
Columns: Deck (full path, middle-truncated) / Field (model's
sort field) / Card (template ordinal) / State / Lapses (count of
times the card lapsed) / Reviews (total reps) / Ivl."
  (list card
        (vector (anki-gt-cards--deck-cell card)
                (or (anki-gt-cards--display-field card) "")
                (anki-gt-cards--template-label card)
                (anki-gt-cards--state-label card)
                (format "%d" (or (alist-get 'lapses card) 0))
                (format "%d" (or (alist-get 'reps card) 0))
                (anki-gt-cards--format-interval card))))

(defun anki-gt-cards--sort-numeric-key (key)
  "Return a `tabulated-list-format' sort predicate on KEY of the card.
KEY is a symbol looked up in the card record via `alist-get';
missing values compare as 0.  Used to give numeric columns a
proper numeric sort instead of string-lexicographic sort."
  (lambda (a b)
    (< (or (alist-get key (car a)) 0)
       (or (alist-get key (car b)) 0))))

;;;; Fetch

(defun anki-gt-cards--fetch (query limit)
  "Fetch up to LIMIT card records matching QUERY.
Returns a cons cell (ALL-IDS . RECORDS) where ALL-IDS is the
full result of `findCards' and RECORDS is the parsed
`cardsInfo' response for the first LIMIT ids."
  (let* ((all (anki-gt-request "findCards" `((query . ,query))))
         (slice (seq-take all limit))
         (records (if slice
                      (anki-gt-request "cardsInfo"
                                       `((cards . ,(vconcat slice))))
                    '())))
    (cons all records)))

(defun anki-gt-cards--refresh ()
  "Re-fetch cards for the current query and repaint the buffer."
  (let* ((result (anki-gt-cards--fetch anki-gt-cards--query
                                       anki-gt-cards-limit))
         (all-ids (car result))
         (records (cdr result)))
    (anki-gt-cards--prefetch-templates records)
    (setq anki-gt-cards--all-ids all-ids)
    (setq anki-gt-cards--fetched (length records))
    (setq tabulated-list-entries (mapcar #'anki-gt-cards--entry records))
    (tabulated-list-print t)
    (anki-gt-cards--update-header)))

(defun anki-gt-cards--update-header ()
  "Refresh mode-line indicators to reflect current query and counts.
Column names live in the header line (populated by
`tabulated-list-init-header').  Query / counts / narrow depth
go into `mode-line-process' so they appear next to the buffer
name in the mode line, mu4e-headers style."
  (setq-local mode-line-process
              (format "  %s  %d/%d%s%s"
                      anki-gt-cards--query
                      anki-gt-cards--fetched
                      (length anki-gt-cards--all-ids)
                      (if (< anki-gt-cards--fetched
                             (length anki-gt-cards--all-ids))
                          " (M)"
                        "")
                      (if anki-gt-cards--query-stack
                          (format " [narrowed×%d]"
                                  (length anki-gt-cards--query-stack))
                        ""))))

;;;; Interactive commands

(defun anki-gt-cards--rerender ()
  "Repaint existing tabulated entries from cached card records.
Unlike `anki-gt-cards--refresh', makes no network call -- use for
display-only invalidations such as toggling `anki-gt-show-ruby'."
  (setq tabulated-list-entries
        (mapcar (lambda (entry) (anki-gt-cards--entry (car entry)))
                tabulated-list-entries))
  (tabulated-list-print t))

(defun anki-gt-cards-refresh ()
  "Re-fetch and repaint the current cards buffer."
  (interactive)
  (anki-gt-cards--refresh))

(declare-function anki-gt-preview-show      "anki-gt-preview" (card &optional source-buffer))
(declare-function anki-gt-preview-close     "anki-gt-preview" ())
(declare-function anki-gt-preview-visible-p "anki-gt-preview" ())

(defun anki-gt-cards-visit-card ()
  "Open (or refresh) the preview pane for the card at point."
  (interactive)
  (let ((card (tabulated-list-get-id)))
    (unless card (user-error "No card at point"))
    (require 'anki-gt-preview)
    (anki-gt-preview-show card (current-buffer))
    (setq anki-gt-cards--last-previewed card)))

(defun anki-gt-cards-close-preview ()
  "Close the preview side-window; keep the cards buffer."
  (interactive)
  (require 'anki-gt-preview)
  (anki-gt-preview-close)
  (setq anki-gt-cards--last-previewed nil))

(defun anki-gt-cards--maybe-preview ()
  "Refresh the preview if it is visible and point moved to a new card.
Runs from `post-command-hook' in cards buffers when
`anki-gt-cards-preview-auto' is non-nil.  Silent no-op when the
preview window is not visible."
  (when (and anki-gt-cards-preview-auto
             (derived-mode-p 'anki-gt-cards-mode)
             (fboundp 'anki-gt-preview-visible-p)
             (anki-gt-preview-visible-p))
    (let ((card (tabulated-list-get-id)))
      (when (and card (not (eq card anki-gt-cards--last-previewed)))
        (anki-gt-preview-show card (current-buffer))
        (setq anki-gt-cards--last-previewed card)))))

(defun anki-gt-cards-edit-in-anki ()
  "Open the note for the card at point in Anki's editor dialog."
  (interactive)
  (let ((card (tabulated-list-get-id)))
    (unless card (user-error "No card at point"))
    (anki-gt-request "guiEditNote"
                     `((note . ,(alist-get 'note card))))
    (message "Opened note %s in Anki" (alist-get 'note card))))

(defun anki-gt-cards-browse-in-anki ()
  "Open the current query in Anki's Card Browser dialog."
  (interactive)
  (unless anki-gt-cards--query
    (user-error "No query in this buffer"))
  (anki-gt-request "guiBrowse"
                   `((query . ,anki-gt-cards--query)))
  (message "Opened query in Anki: %s" anki-gt-cards--query))

(defun anki-gt-cards-open-in-anki ()
  "Open the card at point in Anki's Card Browser and raise Anki.
Focuses the browser on just this card (via `cid:<id>').  Uses
`anki-gt-raise-anki-function', if set, to bring Anki to the
foreground; unset means the browser stays behind Emacs."
  (interactive)
  (let ((card (tabulated-list-get-id)))
    (unless card (user-error "No card at point"))
    (anki-gt-open-card-in-anki card)))

(defun anki-gt-cards--narrow-query (base predicate)
  "Combine BASE query with PREDICATE by whitespace concatenation.
Anki search treats whitespace-separated terms as implicit AND;
PREDICATE is used verbatim, so callers wanting OR or NOT must
supply the full syntax themselves."
  (format "%s %s" base predicate))

(defun anki-gt-cards-search (query)
  "Replace this buffer's query with QUERY and refetch.
When called interactively, prompt with the current query as the
initial input.  Clears the narrow stack."
  (interactive
   (list (read-string "Anki query: "
                      anki-gt-cards--query
                      'anki-gt-cards--query-history)))
  (when (string-empty-p (string-trim query))
    (user-error "Empty query"))
  (setq anki-gt-cards--query-stack nil)
  (setq anki-gt-cards--query query)
  (anki-gt-cards--refresh))

(defun anki-gt-cards-narrow (predicate)
  "Narrow the current query by ANDing PREDICATE onto it.
Pushes the previous query onto the narrow stack so \\[anki-gt-cards-widen] can pop it."
  (interactive
   (list (read-string "Narrow with: "
                      nil
                      'anki-gt-cards--query-history)))
  (when (string-empty-p (string-trim predicate))
    (user-error "Empty predicate"))
  (push anki-gt-cards--query anki-gt-cards--query-stack)
  (setq anki-gt-cards--query
        (anki-gt-cards--narrow-query anki-gt-cards--query predicate))
  (anki-gt-cards--refresh))

(defun anki-gt-cards-widen ()
  "Pop the last narrow: restore the previous query and refetch."
  (interactive)
  (unless anki-gt-cards--query-stack
    (user-error "Nothing to widen"))
  (setq anki-gt-cards--query (pop anki-gt-cards--query-stack))
  (anki-gt-cards--refresh))

(defun anki-gt-cards--goto-card (card-id)
  "Move point to the row whose card-id is CARD-ID.
Falls back to `point-min' when CARD-ID is nil or not in the
current entries.  Returns non-nil when a matching row was found."
  (goto-char (point-min))
  (let (target)
    (when card-id
      (while (and (not target) (not (eobp)))
        (let ((row (tabulated-list-get-id)))
          (when (and row (equal card-id (alist-get 'cardId row)))
            (setq target (point))))
        (forward-line 1)))
    (goto-char (or target (point-min)))
    (and target t)))

(declare-function anki-gt-notes-open       "anki-gt-notes" (query &optional label))
(declare-function anki-gt-notes-mode       "anki-gt-notes" ())
(declare-function anki-gt-notes--refresh   "anki-gt-notes" ())
(declare-function anki-gt-notes--goto-note "anki-gt-notes" (note-id))
;; Bare defvars so the byte-compiler doesn't flag setq on these
;; forward-declared buffer-local vars owned by anki-gt-notes.el.
(defvar anki-gt-notes--query)
(defvar anki-gt-notes--label)
(defvar anki-gt-notes--query-stack)

(defun anki-gt-cards-toggle-to-notes ()
  "Convert this cards buffer in place into a notes buffer.
Reuses the current buffer -- no second buffer is created.  The
query and narrow stack carry over; point moves to the row for
the note the current card belongs to, when that note is in the
result set.  If a notes buffer with the target name already
exists elsewhere it is killed first."
  (interactive)
  (let* ((card (tabulated-list-get-id))
         (target-note-id (and card (alist-get 'note card)))
         (query anki-gt-cards--query)
         (label anki-gt-cards--label)
         (stack anki-gt-cards--query-stack)
         (target-name (format "*anki-gt-notes: %s*" (or label query))))
    (require 'anki-gt-notes)
    (let ((existing (get-buffer target-name)))
      (when (and existing (not (eq existing (current-buffer))))
        (kill-buffer existing)))
    (rename-buffer target-name)
    (anki-gt-notes-mode)
    (setq anki-gt-notes--query query)
    (setq anki-gt-notes--label label)
    (setq anki-gt-notes--query-stack stack)
    (anki-gt-notes--refresh)
    (when target-note-id
      (anki-gt-notes--goto-note target-note-id))))

(defun anki-gt-cards-load-more ()
  "Fetch and append the next batch of cards for the current query."
  (interactive)
  (let* ((remaining (nthcdr anki-gt-cards--fetched anki-gt-cards--all-ids))
         (slice (seq-take remaining anki-gt-cards-limit)))
    (unless slice
      (user-error "All %d cards already loaded" anki-gt-cards--fetched))
    (let* ((records (anki-gt-request "cardsInfo"
                                     `((cards . ,(vconcat slice)))))
           (_ (anki-gt-cards--prefetch-templates records))
           (new-entries (mapcar #'anki-gt-cards--entry records)))
      (setq tabulated-list-entries
            (append tabulated-list-entries new-entries))
      (setq anki-gt-cards--fetched
            (+ anki-gt-cards--fetched (length records)))
      (tabulated-list-print t)
      (anki-gt-cards--update-header)
      (message "Loaded %d more (%d of %d)"
               (length records)
               anki-gt-cards--fetched
               (length anki-gt-cards--all-ids)))))

;;;; Mode definition

(defvar anki-gt-cards-mode-map (make-sparse-keymap)
  "Keymap for `anki-gt-cards-mode'.")

;; Bindings are re-applied on every file load so live sessions pick up
;; new keys without a restart; a bare `defvar' would short-circuit.
(let ((map anki-gt-cards-mode-map))
  (set-keymap-parent map tabulated-list-mode-map)
  (define-key map (kbd "g")   #'anki-gt-cards-refresh)
  (define-key map (kbd "q")   #'quit-window)
  (define-key map (kbd "RET") #'anki-gt-cards-visit-card)
  (define-key map (kbd "e")   #'anki-gt-cards-edit-in-anki)
  (define-key map (kbd "o")   #'anki-gt-cards-browse-in-anki)
  (define-key map (kbd "M")   #'anki-gt-cards-load-more)
  (define-key map (kbd "Q")   #'anki-gt-cards-close-preview)
  (define-key map (kbd "s")   #'anki-gt-cards-search)
  (define-key map (kbd "/")   #'anki-gt-cards-narrow)
  (define-key map (kbd "\\")  #'anki-gt-cards-widen)
  (define-key map (kbd "r")   #'anki-gt-toggle-ruby)
  (define-key map (kbd "a")   #'anki-gt-cards-open-in-anki)
  (define-key map (kbd "v")   #'anki-gt-cards-toggle-to-notes))

(define-derived-mode anki-gt-cards-mode tabulated-list-mode "Anki-Cards"
  "Major mode listing Anki cards for an AnkiConnect search query.
\\{anki-gt-cards-mode-map}"
  (setq tabulated-list-format
        (vector `("Deck"    ,anki-gt-cards-deck-width  t)
                `("Field"   ,anki-gt-cards-field-width t)
                '("Card"     8 t)
                '("State"    6 t)
                `("Lapses"   7 ,(anki-gt-cards--sort-numeric-key 'lapses))
                `("Reviews"  8 ,(anki-gt-cards--sort-numeric-key 'reps))
                `("Ivl"      5 ,(anki-gt-cards--sort-numeric-key 'interval))))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (setq-local revert-buffer-function
              (lambda (&rest _ignore) (anki-gt-cards--refresh)))
  (add-hook 'post-command-hook #'anki-gt-cards--maybe-preview nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun anki-gt-cards-open (query &optional label)
  "Open a card-list buffer for the AnkiConnect search QUERY.
LABEL, when non-nil, is used in the buffer name so multiple
queries can coexist without collision (typically the deck name
the query was scoped to).  The buffer is shown in the current
window (via `pop-to-buffer-same-window'), matching the intent
of RET on a deck line: replace the decks view in place."
  (let* ((name (or label query))
         (buffer (get-buffer-create (format "*anki-gt: %s*" name))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'anki-gt-cards-mode)
        (anki-gt-cards-mode))
      (setq anki-gt-cards--query query)
      (setq anki-gt-cards--label label)
      (anki-gt-cards--refresh))
    (pop-to-buffer-same-window buffer)))

(provide 'anki-gt-cards)

;;; anki-gt-cards.el ends here
