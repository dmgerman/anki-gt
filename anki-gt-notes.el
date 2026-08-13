;;; anki-gt-notes.el --- Note-list buffer for anki-gt  -*- lexical-binding: t; -*-

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

;; Note-list buffer for anki-gt.  Displays notes matching an
;; AnkiConnect search query in a tabulated view, one row per note
;; (deduplicated across cards).  The preview pane is locked to
;; fields view -- notes have no single rendered answer to show.
;;
;; Toggle to the paired cards buffer with \\[anki-gt-notes-toggle-to-cards]
;; and back with \\[anki-gt-cards-toggle-to-notes] in the cards
;; buffer.  Point is preserved across toggles by mapping the current
;; note to its first card and vice versa.

;;; Code:

(require 'anki-gt)
(require 'tabulated-list)
(require 'seq)
(require 'cl-lib)
(require 'subr-x)

(declare-function anki-gt-preview-show-note "anki-gt-preview" (note &optional source-buffer))
(declare-function anki-gt-preview-close     "anki-gt-preview" ())
(declare-function anki-gt-preview-visible-p "anki-gt-preview" ())

;;;; Customization

(defcustom anki-gt-notes-limit 500
  "Maximum number of notes fetched from `notesInfo' per batch.
The full `findNotes' ID list is always retrieved; this option
only caps how many notes are turned into display rows at a
time.  Use \\<anki-gt-notes-mode-map>\\[anki-gt-notes-load-more] to load the next batch."
  :type 'integer
  :group 'anki-gt)

(defcustom anki-gt-notes-field-width 40
  "Display width, in columns, of the note-field column."
  :type 'integer
  :group 'anki-gt)

(defcustom anki-gt-notes-preview-auto t
  "When non-nil, moving point in a notes buffer refreshes the preview.
Fires only while the preview window is already visible; never
spontaneously opens it."
  :type 'boolean
  :group 'anki-gt)

;;;; Buffer-local state

(defvar-local anki-gt-notes--query nil
  "The AnkiConnect search query displayed in this buffer.")

(defvar-local anki-gt-notes--label nil
  "Human-readable label for this query (e.g. a deck name).")

(defvar-local anki-gt-notes--all-ids nil
  "Full list of note IDs matching `anki-gt-notes--query'.")

(defvar-local anki-gt-notes--fetched 0
  "Count of notes already fetched via `notesInfo' and displayed.")

(defvar-local anki-gt-notes--last-previewed nil
  "The note record most recently sent to the preview pane.")

(defvar-local anki-gt-notes--query-stack nil
  "Stack of previous queries in this buffer.
Pushed by `anki-gt-notes-narrow', popped by `anki-gt-notes-widen'.")

;; Reuse the cards-buffer history so `s' completions are shared
;; regardless of which list buffer the user was in last.
(defvar anki-gt-cards--query-history)

;;;; Entry construction

(defun anki-gt-notes--field (note)
  "Return NOTE's display-field value (first field, HTML-stripped).
Notes do not carry a `fieldOrder' the way cards do, so we
default to the field with order 0.  A future refinement could
consult the model's `sortf' via `findModelsByName' and cache."
  (anki-gt-field-at-order note 0))

(defun anki-gt-notes--entry (note)
  "Convert a NOTE alist to a `tabulated-list-mode' entry.
Columns: Field / Model / Tags / Cards-count.  The row's ID is
the NOTE alist itself so commands can retrieve it via
`tabulated-list-get-id' without a side hash table."
  (list note
        (vector (or (anki-gt-notes--field note) "")
                (or (alist-get 'modelName note) "")
                (mapconcat #'identity (alist-get 'tags note) " ")
                (format "%d" (length (alist-get 'cards note))))))

;;;; Fetch

(defun anki-gt-notes--fetch (query limit)
  "Fetch up to LIMIT note records matching QUERY.
Returns (ALL-IDS . RECORDS) where ALL-IDS is the full result of
`findNotes' and RECORDS is the parsed `notesInfo' response for
the first LIMIT ids."
  (let* ((all (anki-gt-request "findNotes" `((query . ,query))))
         (slice (seq-take all limit))
         (records (if slice
                      (anki-gt-request "notesInfo"
                                       `((notes . ,(vconcat slice))))
                    '())))
    (cons all records)))

(defun anki-gt-notes--refresh ()
  "Re-fetch notes for the current query and repaint the buffer."
  (let* ((result (anki-gt-notes--fetch anki-gt-notes--query
                                       anki-gt-notes-limit))
         (all-ids (car result))
         (records (cdr result)))
    (setq anki-gt-notes--all-ids all-ids)
    (setq anki-gt-notes--fetched (length records))
    (setq tabulated-list-entries
          (mapcar #'anki-gt-notes--entry records))
    (tabulated-list-print t)
    (anki-gt-notes--update-header)))

(defun anki-gt-notes--rerender ()
  "Repaint existing tabulated entries from cached note records.
Unlike `anki-gt-notes--refresh', makes no network call -- use
for display-only invalidations such as toggling
`anki-gt-show-ruby'."
  (setq tabulated-list-entries
        (mapcar (lambda (entry) (anki-gt-notes--entry (car entry)))
                tabulated-list-entries))
  (tabulated-list-print t))

(defun anki-gt-notes--update-header ()
  "Refresh mode-line indicators for the current query and counts."
  (setq-local mode-line-process
              (format "  %s  %d/%d%s%s"
                      anki-gt-notes--query
                      anki-gt-notes--fetched
                      (length anki-gt-notes--all-ids)
                      (if (< anki-gt-notes--fetched
                             (length anki-gt-notes--all-ids))
                          " (M)"
                        "")
                      (if anki-gt-notes--query-stack
                          (format " [narrowed×%d]"
                                  (length anki-gt-notes--query-stack))
                        ""))))

;;;; Point navigation across buffers

(defun anki-gt-notes--goto-note (note-id)
  "Move point to the row whose note-id is NOTE-ID.
Falls back to `point-min' when NOTE-ID is nil or not in the
current entries.  Returns non-nil when a matching row was found."
  (goto-char (point-min))
  (let (target)
    (when note-id
      (while (and (not target) (not (eobp)))
        (let ((row (tabulated-list-get-id)))
          (when (and row (equal note-id (alist-get 'noteId row)))
            (setq target (point))))
        (forward-line 1)))
    (goto-char (or target (point-min)))
    (and target t)))

;;;; Preview integration

(defun anki-gt-notes-visit-note ()
  "Open (or refresh) the preview pane for the note at point."
  (interactive)
  (let ((note (tabulated-list-get-id)))
    (unless note (user-error "No note at point"))
    (require 'anki-gt-preview)
    (anki-gt-preview-show-note note (current-buffer))
    (setq anki-gt-notes--last-previewed note)))

(defun anki-gt-notes-close-preview ()
  "Close the preview side-window; keep the notes buffer."
  (interactive)
  (require 'anki-gt-preview)
  (anki-gt-preview-close)
  (setq anki-gt-notes--last-previewed nil))

(defun anki-gt-notes--maybe-preview ()
  "Refresh the preview if visible and point moved to a new note.
Runs from `post-command-hook' when
`anki-gt-notes-preview-auto' is non-nil."
  (when (and anki-gt-notes-preview-auto
             (derived-mode-p 'anki-gt-notes-mode)
             (fboundp 'anki-gt-preview-visible-p)
             (anki-gt-preview-visible-p))
    (let ((note (tabulated-list-get-id)))
      (when (and note (not (eq note anki-gt-notes--last-previewed)))
        (anki-gt-preview-show-note note (current-buffer))
        (setq anki-gt-notes--last-previewed note)))))

;;;; Interactive commands

(defun anki-gt-notes-refresh ()
  "Re-fetch and repaint the current notes buffer."
  (interactive)
  (anki-gt-notes--refresh))

(defun anki-gt-notes-edit-in-anki ()
  "Open the note at point in Anki's editor dialog."
  (interactive)
  (let ((note (tabulated-list-get-id)))
    (unless note (user-error "No note at point"))
    (anki-gt-request "guiEditNote"
                     `((note . ,(alist-get 'noteId note))))
    (anki-gt-raise-anki)
    (message "Opened note %s in Anki" (alist-get 'noteId note))))

(defun anki-gt-notes-browse-in-anki ()
  "Open the current query in Anki's Card Browser dialog."
  (interactive)
  (unless anki-gt-notes--query
    (user-error "No query in this buffer"))
  (anki-gt-request "guiBrowse" `((query . ,anki-gt-notes--query)))
  (anki-gt-raise-anki)
  (message "Opened query in Anki: %s" anki-gt-notes--query))

(defun anki-gt-notes-open-in-anki ()
  "Focus Anki's Card Browser on the note at point.
Filters via `nid:<id>' -- shows every card of this note."
  (interactive)
  (let ((note (tabulated-list-get-id)))
    (unless note (user-error "No note at point"))
    (let ((nid (alist-get 'noteId note)))
      (anki-gt-request "guiBrowse" `((query . ,(format "nid:%s" nid))))
      (anki-gt-raise-anki)
      (message "Focused note %s in Anki" nid))))

(defun anki-gt-notes-load-more ()
  "Fetch and append the next batch of notes for the current query."
  (interactive)
  (let* ((remaining (nthcdr anki-gt-notes--fetched anki-gt-notes--all-ids))
         (slice (seq-take remaining anki-gt-notes-limit)))
    (unless slice
      (user-error "All %d notes already loaded" anki-gt-notes--fetched))
    (let* ((records (anki-gt-request "notesInfo"
                                     `((notes . ,(vconcat slice)))))
           (new-entries (mapcar #'anki-gt-notes--entry records)))
      (setq tabulated-list-entries
            (append tabulated-list-entries new-entries))
      (setq anki-gt-notes--fetched
            (+ anki-gt-notes--fetched (length records)))
      (tabulated-list-print t)
      (anki-gt-notes--update-header)
      (message "Loaded %d more (%d of %d)"
               (length records)
               anki-gt-notes--fetched
               (length anki-gt-notes--all-ids)))))

;;;; Query composition (mirrors anki-gt-cards)

(defun anki-gt-notes-search (query)
  "Replace this buffer's query with QUERY and refetch.
When called interactively, prompts with the current query as
the initial input.  Clears the narrow stack."
  (interactive
   (list (read-string "Anki query: "
                      anki-gt-notes--query
                      'anki-gt-cards--query-history)))
  (when (string-empty-p (string-trim query))
    (user-error "Empty query"))
  (setq anki-gt-notes--query-stack nil)
  (setq anki-gt-notes--query query)
  (anki-gt-notes--refresh))

(defun anki-gt-notes-narrow (predicate)
  "Narrow the current query by ANDing PREDICATE onto it."
  (interactive
   (list (read-string "Narrow with: "
                      nil 'anki-gt-cards--query-history)))
  (when (string-empty-p (string-trim predicate))
    (user-error "Empty predicate"))
  (push anki-gt-notes--query anki-gt-notes--query-stack)
  (setq anki-gt-notes--query
        (format "%s %s" anki-gt-notes--query predicate))
  (anki-gt-notes--refresh))

(defun anki-gt-notes-widen ()
  "Pop the last narrow: restore the previous query and refetch."
  (interactive)
  (unless anki-gt-notes--query-stack
    (user-error "Nothing to widen"))
  (setq anki-gt-notes--query (pop anki-gt-notes--query-stack))
  (anki-gt-notes--refresh))

;;;; Toggle to cards buffer

(declare-function anki-gt-cards-open       "anki-gt-cards" (query &optional label))
(declare-function anki-gt-cards-mode       "anki-gt-cards" ())
(declare-function anki-gt-cards--refresh   "anki-gt-cards" ())
(declare-function anki-gt-cards--goto-card "anki-gt-cards" (card-id))
;; Bare defvars so the byte-compiler doesn't flag setq on these
;; forward-declared buffer-local vars owned by anki-gt-cards.el.
(defvar anki-gt-cards--query)
(defvar anki-gt-cards--label)
(defvar anki-gt-cards--query-stack)

(defun anki-gt-notes-toggle-to-cards ()
  "Convert this notes buffer in place into a cards buffer.
Reuses the current buffer -- no second buffer is created.  The
query and narrow stack carry over; point moves to the row for
the first card of the note under point, when that card is in the
result set.  If a cards buffer with the target name already
exists elsewhere it is killed first."
  (interactive)
  (let* ((note (tabulated-list-get-id))
         (target-card-id
          (and note
               (let ((cards (alist-get 'cards note)))
                 (cond
                  ((listp cards) (car cards))
                  ((vectorp cards) (and (> (length cards) 0)
                                        (aref cards 0)))))))
         (query anki-gt-notes--query)
         (label anki-gt-notes--label)
         (stack anki-gt-notes--query-stack)
         (target-name (format "*anki-gt: %s*" (or label query))))
    (require 'anki-gt-cards)
    (let ((existing (get-buffer target-name)))
      (when (and existing (not (eq existing (current-buffer))))
        (kill-buffer existing)))
    (rename-buffer target-name)
    (anki-gt-cards-mode)
    (setq anki-gt-cards--query query)
    (setq anki-gt-cards--label label)
    (setq anki-gt-cards--query-stack stack)
    (anki-gt-cards--refresh)
    (when target-card-id
      (anki-gt-cards--goto-card target-card-id))))

;;;; Mode definition

(defvar anki-gt-notes-mode-map (make-sparse-keymap)
  "Keymap for `anki-gt-notes-mode'.")

;; Bindings are re-applied on every file load so live sessions pick up
;; new keys without a restart; a bare `defvar' would short-circuit.
(let ((map anki-gt-notes-mode-map))
  (set-keymap-parent map tabulated-list-mode-map)
  (define-key map (kbd "g")   #'anki-gt-notes-refresh)
  (define-key map (kbd "q")   #'quit-window)
  (define-key map (kbd "RET") #'anki-gt-notes-visit-note)
  (define-key map (kbd "e")   #'anki-gt-notes-edit-in-anki)
  (define-key map (kbd "o")   #'anki-gt-notes-browse-in-anki)
  (define-key map (kbd "a")   #'anki-gt-notes-open-in-anki)
  (define-key map (kbd "M")   #'anki-gt-notes-load-more)
  (define-key map (kbd "Q")   #'anki-gt-notes-close-preview)
  (define-key map (kbd "s")   #'anki-gt-notes-search)
  (define-key map (kbd "/")   #'anki-gt-notes-narrow)
  (define-key map (kbd "\\")  #'anki-gt-notes-widen)
  (define-key map (kbd "r")   #'anki-gt-toggle-ruby)
  (define-key map (kbd "v")   #'anki-gt-notes-toggle-to-cards))

(define-derived-mode anki-gt-notes-mode tabulated-list-mode "Anki-Notes"
  "Major mode listing Anki notes for an AnkiConnect search query.
\\{anki-gt-notes-mode-map}"
  (setq tabulated-list-format
        (vector `("Field" ,anki-gt-notes-field-width t)
                '("Model"  20 t)
                '("Tags"   20 t)
                '("Cards"   6 t)))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (setq-local revert-buffer-function
              (lambda (&rest _ignore) (anki-gt-notes--refresh)))
  (add-hook 'post-command-hook #'anki-gt-notes--maybe-preview nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun anki-gt-notes-open (query &optional label)
  "Open a note-list buffer for the AnkiConnect search QUERY.
LABEL, when non-nil, is used in the buffer name so multiple
queries can coexist (typically the deck name the query was
scoped to).  The buffer is shown in the current window (via
`pop-to-buffer-same-window'), matching the intent of RET on a
deck line: replace the decks view in place."
  (let* ((name (or label query))
         (buffer (get-buffer-create (format "*anki-gt-notes: %s*" name))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'anki-gt-notes-mode)
        (anki-gt-notes-mode))
      (setq anki-gt-notes--query query)
      (setq anki-gt-notes--label label)
      (anki-gt-notes--refresh))
    (pop-to-buffer-same-window buffer)
    buffer))

(provide 'anki-gt-notes)

;;; anki-gt-notes.el ends here
