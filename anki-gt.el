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
