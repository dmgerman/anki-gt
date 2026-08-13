;;; anki-gt.el --- Emacs client for AnkiConnect  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>

;; Author: Daniel M. German <dmg@turingmachine.org>
;; Assisted-by: Claude:claude-opus-4-7
;; Maintainer: Daniel M. German <dmg@turingmachine.org>
;; Keywords: convenience, tools
;; URL: https://github.com/dmgerman/anki-gt
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs client for the AnkiConnect add-on
;; (https://git.sr.ht/~foosoft/anki-connect), which exposes Anki's
;; internals over a local HTTP JSON API.
;;
;; This file contains the transport layer only.  Higher-level browsing
;; and editing UI are built on top of `anki-gt-request'.
;;
;; Request envelope: { action, version, params, key? }
;; Response envelope: { result, error }
;;
;; Usage:
;;   (require 'anki-gt)
;;   (anki-gt-check-connection)        ; verify Anki + AnkiConnect are reachable
;;   (anki-gt-request "deckNames")     ; => ("Default" "Japanese" ...)
;;   (anki-gt-request "findCards"
;;                    '((query . "deck:\"Japanese\" is:due")))
;;
;; Errors:
;;   `anki-gt-error'            -- parent of both classes below
;;   `anki-gt-transport-error'  -- network / HTTP / JSON parse failure
;;   `anki-gt-api-error'        -- non-nil `error' field in the response

;;; Code:

(require 'json)
(require 'url)
(require 'subr-x)
(require 'cl-lib)

;;;; Customization

(defgroup anki-gt nil
  "Emacs client for the AnkiConnect add-on."
  :group 'convenience
  :prefix "anki-gt-")

(defcustom anki-gt-endpoint "http://127.0.0.1:8765"
  "URL of the AnkiConnect HTTP server.
The AnkiConnect default is `http://127.0.0.1:8765'.  Change this
only if you have reconfigured AnkiConnect's `webBindAddress' or
`webBindPort' in its add-on configuration."
  :type 'string
  :group 'anki-gt)

(defcustom anki-gt-api-key nil
  "Optional API key sent with each AnkiConnect request.
AnkiConnect does not require a key by default.  Set this if you
have configured `apiKey' in the AnkiConnect add-on configuration."
  :type '(choice (const :tag "None" nil) string)
  :group 'anki-gt)

(defcustom anki-gt-api-version 6
  "AnkiConnect API version sent with each request.
Versions 1 through 6 are defined.  AnkiConnect maintains backward
compatibility, so this should stay at the highest version this
package targets."
  :type 'integer
  :group 'anki-gt)

(defcustom anki-gt-request-timeout 10
  "Seconds to wait for a synchronous AnkiConnect response.
Passed to `url-retrieve-synchronously' as its INHIBIT-COOKIES
sibling TIMEOUT argument."
  :type 'number
  :group 'anki-gt)

(defcustom anki-gt-show-ruby t
  "When non-nil, render ruby (furigana) readings inline as `base[reading]'.
When nil, drop the reading entirely and show only the base text.
Applies uniformly across the preview pane and the field-1 column
of cards buffers.  Toggle interactively with `anki-gt-toggle-ruby'."
  :type 'boolean
  :group 'anki-gt)

(defcustom anki-gt-raise-anki-function nil
  "Function called to bring the Anki application to the foreground.
Invoked with no arguments at the end of
`anki-gt-open-card-in-anki', because AnkiConnect does not raise
Anki's window on its own.  Set to nil (the default) to skip
window activation.

Example for macOS via osascript:
  (setq anki-gt-raise-anki-function #\\='anki-gt-raise-anki-macos)"
  :type '(choice (const :tag "Do nothing" nil) function)
  :group 'anki-gt)

;;;###autoload
(defun anki-gt-toggle-ruby ()
  "Toggle inline display of ruby (furigana) readings.
Flips `anki-gt-show-ruby' and refreshes the visible preview and
the current cards buffer, if any.  Other cards buffers pick up
the new setting on their next repaint."
  (interactive)
  (setq anki-gt-show-ruby (not anki-gt-show-ruby))
  (when (fboundp 'anki-gt-preview-visible-p)
    (when (anki-gt-preview-visible-p)
      (with-current-buffer (get-buffer "*anki-gt-preview*")
        (when (fboundp 'anki-gt-preview-refresh)
          (anki-gt-preview-refresh)))))
  (when (and (derived-mode-p 'anki-gt-cards-mode)
             (fboundp 'anki-gt-cards--rerender))
    (anki-gt-cards--rerender))
  (message "anki-gt ruby: %s" (if anki-gt-show-ruby "on" "off")))

;;;; Errors

(define-error 'anki-gt-error
  "anki-gt error")

(define-error 'anki-gt-transport-error
  "anki-gt transport error"
  'anki-gt-error)

(define-error 'anki-gt-api-error
  "anki-gt API error"
  'anki-gt-error)

;;;; JSON helpers

(defun anki-gt--json-encode (object)
  "Encode OBJECT to a JSON string.
Uses `json-serialize' when available (Emacs 27+ native JSON),
falling back to `json-encode' otherwise.  Alists become JSON
objects and lists become JSON arrays."
  (if (fboundp 'json-serialize)
      (json-serialize object :null-object :null :false-object :false)
    (let ((json-null :null)
          (json-false :false))
      (json-encode object))))

(defun anki-gt--json-decode (string)
  "Decode STRING as a JSON document.
Uses `json-parse-string' when available (Emacs 27+ native JSON),
falling back to `json-read-from-string' otherwise.  JSON objects
become alists, arrays become lists, `null' becomes nil, and
`false' becomes `:false' so it stays distinguishable from a
missing key."
  (if (fboundp 'json-parse-string)
      (json-parse-string string
                         :object-type 'alist
                         :array-type 'list
                         :null-object nil
                         :false-object :false)
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-null nil)
          (json-false :false))
      (json-read-from-string string))))

;;;; Envelope construction

(defun anki-gt--build-envelope (action params)
  "Build the AnkiConnect request envelope for ACTION and PARAMS.
PARAMS is an alist of parameter names to values, or nil for
actions that take no parameters.  The envelope always carries
`version' equal to `anki-gt-api-version', and `key' when
`anki-gt-api-key' is non-nil."
  (let ((env `((action . ,action)
               (version . ,anki-gt-api-version))))
    (when params
      (setq env (append env `((params . ,params)))))
    (when anki-gt-api-key
      (setq env (append env `((key . ,anki-gt-api-key)))))
    env))

;;;; Response parsing

(defun anki-gt--parse-response-buffer (buffer)
  "Extract and decode the JSON body from a `url-retrieve' BUFFER.
Signals `anki-gt-transport-error' on unreachable server, missing
body, non-2xx HTTP status, or JSON parse failure.  Returns the
decoded response as an alist with `result' and `error' keys."
  (unwind-protect
      (with-current-buffer buffer
        (when (zerop (buffer-size))
          (signal 'anki-gt-transport-error
                  (list "Could not reach AnkiConnect (empty response)"
                        anki-gt-endpoint)))
        (goto-char (point-min))
        (let ((status-line (buffer-substring-no-properties
                            (point) (line-end-position))))
          (unless (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
            (signal 'anki-gt-transport-error
                    (list "No HTTP status line in response" status-line)))
          (let ((code (string-to-number (match-string 1))))
            (unless (and (>= code 200) (< code 300))
              (signal 'anki-gt-transport-error
                      (list (format "HTTP %d from AnkiConnect" code)
                            status-line)))))
        (goto-char (point-min))
        (unless (re-search-forward "\r?\n\r?\n" nil t)
          (signal 'anki-gt-transport-error
                  (list "No body in AnkiConnect response")))
        (let ((body (buffer-substring-no-properties (point) (point-max))))
          (condition-case err
              (anki-gt--json-decode body)
            (error
             (signal 'anki-gt-transport-error
                     (list "Malformed JSON in AnkiConnect response"
                           (error-message-string err)
                           body))))))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

;;;; Public API

;;;###autoload
(defun anki-gt-request (action &optional params)
  "Send ACTION to AnkiConnect and return its result.
PARAMS is an alist of parameter names to values (or nil).
Signals `anki-gt-api-error' if the server returns a non-nil
`error' field, or `anki-gt-transport-error' on network, HTTP,
or JSON failure.  Blocks the calling thread until the response
arrives or `anki-gt-request-timeout' seconds elapse."
  (let* ((envelope (anki-gt--build-envelope action params))
         (payload (anki-gt--json-encode envelope))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string payload 'utf-8))
         (buffer (condition-case err
                     (url-retrieve-synchronously
                      anki-gt-endpoint t nil anki-gt-request-timeout)
                   (error
                    (signal 'anki-gt-transport-error
                            (list "Could not reach AnkiConnect"
                                  anki-gt-endpoint
                                  (error-message-string err)))))))
    (unless buffer
      (signal 'anki-gt-transport-error
              (list "AnkiConnect request timed out" anki-gt-endpoint)))
    (let* ((response (anki-gt--parse-response-buffer buffer))
           (err (alist-get 'error response))
           (result (alist-get 'result response)))
      (when err
        (signal 'anki-gt-api-error (list err action)))
      result)))

;;;###autoload
(defun anki-gt-check-connection ()
  "Verify that AnkiConnect is reachable and speaks a compatible version.
Returns the reported API version on success.  Signals
`anki-gt-transport-error' if AnkiConnect is unreachable, or a
plain error if its reported version is lower than
`anki-gt-api-version'."
  (interactive)
  (let ((reported (anki-gt-request "version")))
    (unless (and (integerp reported) (>= reported anki-gt-api-version))
      (error "AnkiConnect reports version %S, need >= %d"
             reported anki-gt-api-version))
    (when (called-interactively-p 'interactive)
      (message "AnkiConnect OK (API version %d)" reported))
    reported))

;;;; Media directory (cached)

(defvar anki-gt--media-dir-cache nil
  "Cached value of the collection.media directory path.
Populated on first call to `anki-gt-media-dir'; cleared by
`anki-gt-media-dir-invalidate'.")

(defun anki-gt-media-dir ()
  "Return the absolute path to Anki's collection.media folder.
Cached across the session; invalidate with
`anki-gt-media-dir-invalidate' after switching Anki profiles."
  (or anki-gt--media-dir-cache
      (setq anki-gt--media-dir-cache
            (anki-gt-request "getMediaDirPath"))))

;;;###autoload
(defun anki-gt-media-dir-invalidate ()
  "Discard the cached media-directory path.
Useful after switching Anki profiles or moving the collection."
  (interactive)
  (setq anki-gt--media-dir-cache nil)
  (when (called-interactively-p 'interactive)
    (message "anki-gt media-dir cache cleared")))

(defun anki-gt-truncate-middle (str max-width)
  "Return STR truncated to MAX-WIDTH by eliding the middle with `…'.
When STR contains the deck separator `::' and its final segment
fits, preserves the trailing `…::LAST' so the leaf name stays
intact and readable.  Falls back to a naive middle-elide when
no separator is present or the last segment alone exceeds
MAX-WIDTH.  Returns STR unchanged when its display width is at
most MAX-WIDTH."
  (let ((str (or str "")))
    (cond
     ((<= (string-width str) max-width) str)
     (t
      (let* ((parts (split-string str "::"))
             (last (car (last parts))))
        (if (or (= (length parts) 1)
                (>= (string-width last) (1- max-width)))
            ;; No separator, or the leaf alone won't fit -- naive.
            (truncate-string-to-width str max-width nil nil "…")
          (let* ((tail (concat "…::" last))
                 (prefix-max (- max-width (string-width tail))))
            (if (<= prefix-max 0)
                tail
              (concat (truncate-string-to-width str prefix-max nil nil "")
                      tail)))))))))

(defun anki-gt-transform-ruby (str)
  "Rewrite `<ruby>' markup in STR per `anki-gt-show-ruby'.
With ruby on, `<rt>reading</rt>' becomes `[reading]' inline.
With ruby off, the reading is dropped, only the base survives,
and any pre-baked literal `kanji[reading]' patterns in the raw
text are stripped via `anki-gt-strip-literal-furigana'.
Regex-based, not a full parser -- adequate for list-buffer
field display, where noisy edge cases fall through to the
generic tag stripper."
  (let* ((s (replace-regexp-in-string "<rp>[^<]*</rp>" "" str))
         (s (replace-regexp-in-string
             "<rt>\\([^<]*\\)</rt>"
             (if anki-gt-show-ruby "[\\1]" "")
             s))
         (s (replace-regexp-in-string "</?rb>" "" s))
         (s (replace-regexp-in-string "</?ruby[^>]*>" "" s))
         (s (if anki-gt-show-ruby s (anki-gt-strip-literal-furigana s))))
    s))

(defun anki-gt-strip-html (str)
  "Remove HTML tags, [sound:] tags, and collapse whitespace in STR.
Ruby markup is rewritten first by `anki-gt-transform-ruby' so
its behaviour tracks `anki-gt-show-ruby'.  Also decodes the small
set of HTML entities commonly used in Anki fields.  Returns the
empty string when STR is nil."
  (thread-last (or str "")
    (anki-gt-transform-ruby)
    (replace-regexp-in-string "\\[sound:[^]]+\\]" "")
    (replace-regexp-in-string "<[^>]+>" "")
    (replace-regexp-in-string "&nbsp;" " ")
    (replace-regexp-in-string "&amp;" "&")
    (replace-regexp-in-string "&lt;" "<")
    (replace-regexp-in-string "&gt;" ">")
    (replace-regexp-in-string "&quot;" "\"")
    (replace-regexp-in-string "\\s-+" " ")
    (string-trim)))

(defun anki-gt-field-at-order (record order)
  "Return RECORD's field value at ORDER, HTML-stripped.
RECORD may be a card or a note; both expose a `fields' alist.
ORDER is the integer index of the field.  Returns the empty
string when no field with that order exists."
  (let ((found (cl-find-if
                (lambda (pair)
                  (eq order (alist-get 'order (cdr pair))))
                (alist-get 'fields record))))
    (anki-gt-strip-html (and found (alist-get 'value (cdr found))))))

;;;; Literal furigana stripping

(defun anki-gt-strip-literal-furigana (str)
  "In STR, drop pre-baked furigana in the `base[reading]' format.
Some Anki templates (Yomichan imports, MIA decks, Jalup cards)
bake readings directly into the field text rather than using
`<ruby>' HTML, so the DOM ruby handler cannot see them.  This
text-level pass strips them when `anki-gt-show-ruby' is off.

Matches:

  BASE `[' READING (`;' MARKER)? `]'

BASE is one or more CJK ideographs or kana (so `出産[しゅっさん]',
`する[,する]', and `分[わ,わかる]' all count).  READING is zero
or more kana or commas.  MARKER is optional and consists of
ASCII letters/digits, matching Jalup / MIA markers like `h',
`k2', `o'.  Content that contains English text (`[dog]',
`[toolぐ]', `[exampleれい]') is left alone -- those are
annotations, not furigana."
  (replace-regexp-in-string
   "\\([一-鿯々〆〤ヶヵぁ-ゖァ-ヺー]+\\)\\[[ぁ-ゖァ-ヺー,]*\\(?:;[A-Za-z0-9]+\\)?\\]"
   "\\1"
   (or str "")))

;;;; Raise Anki (window activation)

;;;###autoload
(defun anki-gt-raise-anki ()
  "Invoke `anki-gt-raise-anki-function' if it is set.
Interactive so it can be bound to a key directly for standalone
window activation; called automatically at the end of
`anki-gt-open-card-in-anki'.  A no-op when the variable is nil."
  (interactive)
  (when (functionp anki-gt-raise-anki-function)
    (funcall anki-gt-raise-anki-function)))

;;;###autoload
(defun anki-gt-raise-anki-macos ()
  "Bring the Anki macOS application to the foreground via osascript.
Suitable both as the value of `anki-gt-raise-anki-function' and
as a standalone interactive command."
  (interactive)
  (call-process "osascript" nil 0 nil
                "-e" "tell application \"Anki\" to activate"))

;;;; Open a specific card in Anki's Card Browser

;;;###autoload
(defun anki-gt-open-card-in-anki (card)
  "Open CARD in Anki's Card Browser dialog and raise the window.
Sends `guiBrowse cid:<id>' to filter the browser to just this
card -- with a one-card result set the row is unambiguously in
view.  `guiSelectCard' is called opportunistically to select the
row explicitly, but it is only present in newer AnkiConnect
releases; the resulting `unsupported action' error is swallowed
on older ones.  Finally, `anki-gt-raise-anki' is invoked so the
user sees the result immediately."
  (let ((cid (or (alist-get 'cardId card)
                 (user-error "Card record has no `cardId' field"))))
    (anki-gt-request "guiBrowse" `((query . ,(format "cid:%s" cid))))
    (condition-case _err
        (anki-gt-request "guiSelectCard" `((card . ,cid)))
      (anki-gt-api-error nil))
    (anki-gt-raise-anki)))

;;;; Top-level entry point

;;;###autoload
(defun anki-gt ()
  "Open the main anki-gt buffer.
Loads `anki-gt-main' on demand and calls `anki-gt-main-open'."
  (interactive)
  (require 'anki-gt-main)
  (declare-function anki-gt-main-open "anki-gt-main" ())
  (anki-gt-main-open))

(provide 'anki-gt)

;;; anki-gt.el ends here
