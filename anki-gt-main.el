;;; anki-gt-main.el --- Main entry buffer for anki-gt  -*- lexical-binding: t; -*-

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

;; Top-level buffer for anki-gt: connection status, deck listing with
;; hierarchical indentation and per-deck (due / total) counts, and a
;; footer of available actions.
;;
;; Entry point: `M-x anki-gt'.
;;
;; This slice is read-only navigation.  RET on a deck line will later
;; open a query buffer scoped to that deck; for now it messages the
;; query that would be run.

;;; Code:

(require 'anki-gt)
(require 'subr-x)
(require 'cl-lib)

;;;; Constants

(defconst anki-gt-main--deck-separator "::"
  "String that separates parent from child in Anki deck names.
Single colons may appear inside a segment and must not be split.")

(defconst anki-gt-main--buffer-name "*anki-gt*"
  "Name of the main anki-gt buffer.")

(defconst anki-gt-main--name-column 60
  "Column at which deck stat numbers are right-aligned.")

;;;; Faces

(defgroup anki-gt-main nil
  "Faces and options for the main anki-gt buffer."
  :group 'anki-gt
  :prefix "anki-gt-main-")

(defface anki-gt-main-header-face
  '((t :inherit font-lock-comment-face))
  "Face for the header line at the top of the anki-gt buffer."
  :group 'anki-gt-main)

(defface anki-gt-main-section-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for section labels (Decks, Actions) in the anki-gt buffer."
  :group 'anki-gt-main)

(defface anki-gt-main-deck-face
  '((t :inherit default))
  "Face for deck names in the anki-gt buffer."
  :group 'anki-gt-main)

(defface anki-gt-main-count-face
  '((t :inherit font-lock-constant-face))
  "Face for the numeric counts next to each deck."
  :group 'anki-gt-main)

(defface anki-gt-main-count-due-face
  '((t :inherit font-lock-warning-face))
  "Face for the due count when it is greater than zero."
  :group 'anki-gt-main)

;;;; Deck name helpers

(defun anki-gt-main--split-name (name)
  "Split a full deck NAME on `anki-gt-main--deck-separator'.
A single colon inside a segment is preserved."
  (split-string name anki-gt-main--deck-separator))

(defun anki-gt-main--level (name)
  "Return the nesting level of deck NAME (0 for a top-level deck)."
  (1- (length (anki-gt-main--split-name name))))

(defun anki-gt-main--leaf (name)
  "Return the last segment of deck NAME."
  (car (last (anki-gt-main--split-name name))))

;;;; Data fetch

(defun anki-gt-main--fetch-decks ()
  "Fetch deck names, IDs, and stats from AnkiConnect.
Returns a list of plists sorted so that a parent deck appears
before any of its children.  Each plist has the keys :name, :id,
:level, :leaf, :new, :learn, :review, and :total.  A deck with
no stats (rare, but possible for transient collections) reports
zero for every count.  `:review' corresponds to the \"Due\"
column in Anki's own deck browser."
  (let* ((names-and-ids (anki-gt-request "deckNamesAndIds"))
         ;; getDeckStats' `decks' parameter is an array of NAMES, not IDs.
         (names (mapcar (lambda (p) (symbol-name (car p)))
                        names-and-ids))
         (stats (when names
                  (anki-gt-request "getDeckStats"
                                   `((decks . ,(vconcat names))))))
         (rows (mapcar
                (lambda (pair)
                  (let* ((name (symbol-name (car pair)))
                         (id (cdr pair))
                         (entry (or (alist-get (intern (number-to-string id))
                                               stats)
                                    (alist-get (number-to-string id)
                                               stats nil nil #'equal)))
                         (get (lambda (k) (or (alist-get k entry) 0)))
                         (new (funcall get 'new_count))
                         (learn (funcall get 'learn_count))
                         (review (funcall get 'review_count))
                         (total (funcall get 'total_in_deck)))
                    (list :name name
                          :id id
                          :level (anki-gt-main--level name)
                          :leaf (anki-gt-main--leaf name)
                          :new new
                          :learn learn
                          :review review
                          :total total)))
                names-and-ids)))
    ;; Sorting by full string puts each parent immediately before its
    ;; own subtree and orders siblings alphabetically, so we get the
    ;; correct pre-order traversal for indented display.
    (sort rows (lambda (a b)
                 (string< (plist-get a :name) (plist-get b :name))))))

;;;; Rendering

(defun anki-gt-main--insert-header ()
  "Insert the header line for the anki-gt buffer."
  (let ((endpoint anki-gt-endpoint)
        (version (condition-case _err
                     (anki-gt-request "version")
                   (anki-gt-error 'unknown))))
    (insert (propertize
             (format "AnkiConnect  API v%s  •  %s\n\n"
                     (if (integerp version) (number-to-string version) "?")
                     endpoint)
             'face 'anki-gt-main-header-face))))

(defun anki-gt-main--insert-decks (rows)
  "Insert the Decks section from ROWS (as returned by `anki-gt-main--fetch-decks')."
  (insert (propertize "Decks\n" 'face 'anki-gt-main-section-face))
  (anki-gt-main--insert-column-header)
  (dolist (row rows)
    (anki-gt-main--insert-deck-line row))
  (insert "\n"))

(defun anki-gt-main--insert-column-header ()
  "Insert a right-aligned header labelling the four stat columns.
Labels line up with the numeric columns emitted by
`anki-gt-main--insert-deck-line'."
  (let ((padding (make-string anki-gt-main--name-column ?\ )))
    (insert (propertize
             (format "%s%6s %6s %6s %6s\n"
                     padding "New" "Learn" "Due" "Total")
             'face 'anki-gt-main-header-face))))

(defun anki-gt-main--insert-deck-line (row)
  "Insert one deck line for ROW (a plist from `anki-gt-main--fetch-decks').
Handles double-width characters (e.g. CJK) when aligning the
count columns.  Emits four columns: New, Learn, Due, Total.
The Due column is highlighted when its value is positive."
  (let* ((start (point))
         (level (plist-get row :level))
         (leaf (plist-get row :leaf))
         (new (plist-get row :new))
         (learn (plist-get row :learn))
         (review (plist-get row :review))
         (total (plist-get row :total))
         (indent (make-string (* 2 (1+ level)) ?\ )))
    (insert (propertize (concat indent leaf) 'face 'anki-gt-main-deck-face))
    (let ((visual (string-width (concat indent leaf))))
      (when (< visual anki-gt-main--name-column)
        (insert-char ?\  (- anki-gt-main--name-column visual))))
    (insert (propertize (format "%6d" new)
                        'face 'anki-gt-main-count-face))
    (insert " ")
    (insert (propertize (format "%6d" learn)
                        'face 'anki-gt-main-count-face))
    (insert " ")
    (insert (propertize (format "%6d" review)
                        'face (if (> review 0)
                                  'anki-gt-main-count-due-face
                                'anki-gt-main-count-face)))
    (insert " ")
    (insert (propertize (format "%6d\n" total)
                        'face 'anki-gt-main-count-face))
    (add-text-properties start (point)
                         (list 'anki-gt-deck-name (plist-get row :name)
                               'anki-gt-deck-id (plist-get row :id)
                               'anki-gt-deck-level level))))

(defun anki-gt-main--insert-footer ()
  "Insert the Actions footer for the anki-gt buffer."
  (insert (propertize "Actions\n" 'face 'anki-gt-main-section-face))
  (insert "  g  refresh    n/p  next/previous    RET  open deck    ")
  (insert "?  help    q  quit\n"))

(defun anki-gt-main--render ()
  "Fully repaint the current buffer (must be `anki-gt-main-mode')."
  (let ((inhibit-read-only t)
        (rows (anki-gt-main--fetch-decks))
        (prev-deck (get-text-property (point) 'anki-gt-deck-name)))
    (erase-buffer)
    (anki-gt-main--insert-header)
    (anki-gt-main--insert-decks rows)
    (anki-gt-main--insert-footer)
    (goto-char (point-min))
    (anki-gt-main--goto-deck (or prev-deck
                                 (and rows (plist-get (car rows) :name))))))

(defun anki-gt-main--goto-deck (name)
  "Move point to the line whose deck-name text property equals NAME.
Falls back to the first deck line if NAME is nil or not found."
  (goto-char (point-min))
  (let (target)
    (when name
      (let ((pos (point-min)))
        (while (and (not target)
                    (setq pos (text-property-not-all
                               pos (point-max) 'anki-gt-deck-name nil)))
          (when (equal name (get-text-property pos 'anki-gt-deck-name))
            (setq target pos))
          (setq pos (next-single-property-change
                     pos 'anki-gt-deck-name nil (point-max))))))
    (unless target
      (setq target (text-property-not-all
                    (point-min) (point-max) 'anki-gt-deck-name nil)))
    (when target
      (goto-char target)
      (beginning-of-line))))

;;;; Interactive commands

(defun anki-gt-main-refresh ()
  "Re-fetch deck data and repaint the buffer."
  (interactive)
  (anki-gt-main--render))

(defun anki-gt-main-quit ()
  "Bury the anki-gt buffer."
  (interactive)
  (quit-window))

(defun anki-gt-main-open-at-point ()
  "Placeholder for opening the deck at point in a query buffer.
The query buffer is not implemented yet, so this just echoes the
query that would be run."
  (interactive)
  (let ((name (get-text-property (point) 'anki-gt-deck-name)))
    (if name
        (message "would run: deck:\"%s\"" name)
      (user-error "No deck at point"))))

(defun anki-gt-main-next-line (&optional n)
  "Move point down N lines, staying on lines that carry a deck name.
Lines without a deck name are skipped."
  (interactive "p")
  (anki-gt-main--move-line (or n 1)))

(defun anki-gt-main-previous-line (&optional n)
  "Move point up N lines, staying on lines that carry a deck name."
  (interactive "p")
  (anki-gt-main--move-line (- (or n 1))))

(defun anki-gt-main--move-line (delta)
  "Move by DELTA lines, snapping to the next line that has a deck name.
Signals `end-of-buffer' or `beginning-of-buffer' when no such
line exists in the requested direction."
  (let ((step (if (> delta 0) 1 -1))
        (remaining (abs delta)))
    (while (> remaining 0)
      (let ((start (point)))
        (forward-line step)
        (while (and (not (eobp)) (not (bobp))
                    (not (get-text-property (point) 'anki-gt-deck-name)))
          (forward-line step))
        (unless (get-text-property (point) 'anki-gt-deck-name)
          (goto-char start)
          (if (> step 0) (signal 'end-of-buffer nil)
            (signal 'beginning-of-buffer nil))))
      (setq remaining (1- remaining)))))

;;;; Mode definition

(defvar anki-gt-main-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'anki-gt-main-refresh)
    (define-key map (kbd "q") #'anki-gt-main-quit)
    (define-key map (kbd "n") #'anki-gt-main-next-line)
    (define-key map (kbd "p") #'anki-gt-main-previous-line)
    (define-key map (kbd "RET") #'anki-gt-main-open-at-point)
    (define-key map (kbd "?") #'describe-mode)
    map)
  "Keymap for `anki-gt-main-mode'.")

(define-derived-mode anki-gt-main-mode special-mode "Anki-GT"
  "Top-level buffer for the anki-gt AnkiConnect client.
\\{anki-gt-main-mode-map}"
  (setq truncate-lines t)
  (setq-local revert-buffer-function
              (lambda (&rest _ignore) (anki-gt-main-refresh))))

;;;###autoload
(defun anki-gt-main-open ()
  "Open the main anki-gt buffer.
Called by the top-level `anki-gt' command."
  (interactive)
  (let ((buffer (get-buffer-create anki-gt-main--buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'anki-gt-main-mode)
        (anki-gt-main-mode))
      (anki-gt-main--render))
    (pop-to-buffer buffer)))

(provide 'anki-gt-main)

;;; anki-gt-main.el ends here
