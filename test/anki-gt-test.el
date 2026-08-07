;;; anki-gt-test.el --- Tests for anki-gt         -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT test suite for anki-gt.
;;
;; Unit tests stub `url-retrieve-synchronously' via `cl-letf' so they
;; never touch the network.  Integration tests hit a real AnkiConnect
;; and are gated on the `ANKI_GT_INTEGRATION' environment variable so
;; `make test' remains safe on CI without a running Anki.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anki-gt)
(require 'anki-gt-main)

;;;; Helpers

;; `defvar' declares this as a special (dynamic) variable so that
;; `let'-binding it in `anki-gt-test--with-stubbed-http' remains visible
;; to `anki-gt-test--captured-json' via dynamic scope, even though the
;; file is compiled with lexical binding.
(defvar anki-gt-test--captured-request nil
  "Raw payload sent by the most recent stubbed `url-retrieve-synchronously' call.")

(defun anki-gt-test--response-buffer (body &optional status)
  "Return a fresh buffer that mimics a `url-retrieve' response.
BODY is the raw JSON body string; STATUS is the HTTP status line
minus the `HTTP/1.1 ' prefix (default \"200 OK\")."
  (let ((buf (generate-new-buffer " *anki-gt-test-response*")))
    (with-current-buffer buf
      (set-buffer-multibyte nil)
      (insert (format "HTTP/1.1 %s\r\n" (or status "200 OK")))
      (insert "Content-Type: application/json\r\n")
      (insert "\r\n")
      (insert body))
    buf))

(defmacro anki-gt-test--with-stubbed-http (spec &rest body)
  "Run BODY with `url-retrieve-synchronously' stubbed by SPEC.
SPEC is evaluated on each call and must return a buffer suitable
for `anki-gt--parse-response-buffer'.  The macro also captures the
outgoing request payload into `anki-gt-test--captured-request' so
tests can assert on the envelope."
  (declare (indent 1) (debug (form body)))
  `(let ((anki-gt-test--captured-request nil))
     (cl-letf (((symbol-function 'url-retrieve-synchronously)
                (lambda (&rest _args)
                  (setq anki-gt-test--captured-request
                        (and (boundp 'url-request-data)
                             url-request-data))
                  ,spec)))
       ,@body)))

(defun anki-gt-test--captured-json ()
  "Decode the captured outgoing request payload as JSON."
  (should anki-gt-test--captured-request)
  (anki-gt--json-decode
   (decode-coding-string anki-gt-test--captured-request 'utf-8)))

;;;; Envelope tests

(ert-deftest anki-gt-test-envelope-no-params ()
  "Envelope with no PARAMS omits the `params' key."
  (let ((anki-gt-api-key nil)
        (anki-gt-api-version 6))
    (let ((env (anki-gt--build-envelope "version" nil)))
      (should (equal (alist-get 'action env) "version"))
      (should (equal (alist-get 'version env) 6))
      (should-not (assq 'params env))
      (should-not (assq 'key env)))))

(ert-deftest anki-gt-test-envelope-with-params ()
  "Envelope includes PARAMS when supplied."
  (let ((anki-gt-api-key nil))
    (let* ((params '((query . "deck:Japanese")))
           (env (anki-gt--build-envelope "findCards" params)))
      (should (equal (alist-get 'params env) params)))))

(ert-deftest anki-gt-test-envelope-with-api-key ()
  "Envelope includes `key' when `anki-gt-api-key' is set."
  (let ((anki-gt-api-key "secret"))
    (let ((env (anki-gt--build-envelope "deckNames" nil)))
      (should (equal (alist-get 'key env) "secret")))))

;;;; Request round-trip tests (with stubbed HTTP)

(ert-deftest anki-gt-test-request-returns-result ()
  "`anki-gt-request' returns the decoded `result' field."
  (anki-gt-test--with-stubbed-http
      (anki-gt-test--response-buffer
       "{\"result\":[\"Default\",\"Japanese\"],\"error\":null}")
    (should (equal (anki-gt-request "deckNames")
                   '("Default" "Japanese")))))

(ert-deftest anki-gt-test-request-sends-expected-envelope ()
  "The outgoing payload matches the envelope we asked for."
  (let ((anki-gt-api-key nil)
        (anki-gt-api-version 6))
    (anki-gt-test--with-stubbed-http
        (anki-gt-test--response-buffer
         "{\"result\":6,\"error\":null}")
      (anki-gt-request "version")
      (let ((sent (anki-gt-test--captured-json)))
        (should (equal (alist-get 'action sent) "version"))
        (should (equal (alist-get 'version sent) 6))
        (should-not (assq 'params sent))
        (should-not (assq 'key sent))))))

(ert-deftest anki-gt-test-request-signals-api-error ()
  "`anki-gt-request' signals `anki-gt-api-error' on non-nil `error'."
  (anki-gt-test--with-stubbed-http
      (anki-gt-test--response-buffer
       "{\"result\":null,\"error\":\"deck was not found\"}")
    (let ((err (should-error (anki-gt-request "changeDeck")
                             :type 'anki-gt-api-error)))
      ;; Error data preserves the server message.
      (should (member "deck was not found" (cdr err))))))

(ert-deftest anki-gt-test-api-error-inherits-anki-gt-error ()
  "`anki-gt-api-error' is a subtype of `anki-gt-error'."
  (anki-gt-test--with-stubbed-http
      (anki-gt-test--response-buffer
       "{\"result\":null,\"error\":\"boom\"}")
    (should-error (anki-gt-request "any") :type 'anki-gt-error)))

;;;; Transport failure tests

(ert-deftest anki-gt-test-request-signals-transport-on-http-500 ()
  "Non-2xx HTTP status raises `anki-gt-transport-error'."
  (anki-gt-test--with-stubbed-http
      (anki-gt-test--response-buffer "internal boom" "500 Server Error")
    (should-error (anki-gt-request "version")
                  :type 'anki-gt-transport-error)))

(ert-deftest anki-gt-test-request-signals-transport-on-malformed-json ()
  "Malformed body raises `anki-gt-transport-error'."
  (anki-gt-test--with-stubbed-http
      (anki-gt-test--response-buffer "not-json-at-all")
    (should-error (anki-gt-request "version")
                  :type 'anki-gt-transport-error)))

(ert-deftest anki-gt-test-request-signals-transport-on-empty-buffer ()
  "An empty response buffer (connection refused on some builds) is a transport error."
  (anki-gt-test--with-stubbed-http
      (generate-new-buffer " *anki-gt-test-empty*")
    (let ((err (should-error (anki-gt-request "version")
                             :type 'anki-gt-transport-error)))
      (should (cl-some (lambda (e)
                         (and (stringp e) (string-match-p "empty response" e)))
                       (cdr err))))))

(ert-deftest anki-gt-test-request-signals-transport-on-connection-error ()
  "`url-retrieve-synchronously' raising is turned into a transport error."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest _args)
               (error "Connection refused"))))
    (should-error (anki-gt-request "version")
                  :type 'anki-gt-transport-error)))

(ert-deftest anki-gt-test-request-signals-transport-on-timeout ()
  "A nil return from `url-retrieve-synchronously' becomes a transport error."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest _args) nil)))
    (should-error (anki-gt-request "version")
                  :type 'anki-gt-transport-error)))

;;;; Version check tests

(ert-deftest anki-gt-test-check-connection-accepts-current-version ()
  "`anki-gt-check-connection' returns the reported version on success."
  (let ((anki-gt-api-version 6))
    (anki-gt-test--with-stubbed-http
        (anki-gt-test--response-buffer
         "{\"result\":6,\"error\":null}")
      (should (equal (anki-gt-check-connection) 6)))))

(ert-deftest anki-gt-test-check-connection-rejects-older-version ()
  "`anki-gt-check-connection' errors when server is older than required."
  (let ((anki-gt-api-version 6))
    (anki-gt-test--with-stubbed-http
        (anki-gt-test--response-buffer
         "{\"result\":4,\"error\":null}")
      (should-error (anki-gt-check-connection)))))

;;;; Integration tests (opt-in)
;;
;; These run only when ANKI_GT_INTEGRATION is set in the environment
;; AND Anki is reachable at `anki-gt-endpoint'.  They are otherwise
;; skipped so CI stays green without a live Anki.

(defun anki-gt-test--integration-enabled-p ()
  "Return non-nil if integration tests should run."
  (and (getenv "ANKI_GT_INTEGRATION") t))

(ert-deftest anki-gt-test-integration-version ()
  "Live AnkiConnect responds to `version'."
  (skip-unless (anki-gt-test--integration-enabled-p))
  (let ((v (anki-gt-request "version")))
    (should (integerp v))
    (should (>= v 6))))

(ert-deftest anki-gt-test-integration-deck-names ()
  "Live AnkiConnect responds to `deckNames' with a list of strings."
  (skip-unless (anki-gt-test--integration-enabled-p))
  (let ((decks (anki-gt-request "deckNames")))
    (should (listp decks))
    (dolist (d decks)
      (should (stringp d)))))

;;;; anki-gt-main: deck-name hierarchy helpers

(ert-deftest anki-gt-main-test-split-top-level ()
  "A top-level deck name splits into a single segment."
  (should (equal (anki-gt-main--split-name "Default") '("Default")))
  (should (equal (anki-gt-main--level "Default") 0))
  (should (equal (anki-gt-main--leaf "Default") "Default")))

(ert-deftest anki-gt-main-test-split-nested ()
  "A nested deck name splits on `::' at each level."
  (should (equal (anki-gt-main--split-name "jp::active::video")
                 '("jp" "active" "video")))
  (should (equal (anki-gt-main--level "jp::active::video") 2))
  (should (equal (anki-gt-main--leaf "jp::active::video") "video")))

(ert-deftest anki-gt-main-test-single-colon-inside-segment ()
  "A single colon inside a segment must NOT be treated as a separator.
Real-world example: `other::JLPT Tango N3:audio' is a two-level deck
whose leaf segment is `JLPT Tango N3:audio'."
  (should (equal (anki-gt-main--split-name "other::JLPT Tango N3:audio")
                 '("other" "JLPT Tango N3:audio")))
  (should (equal (anki-gt-main--level "other::JLPT Tango N3:audio") 1))
  (should (equal (anki-gt-main--leaf "other::JLPT Tango N3:audio")
                 "JLPT Tango N3:audio")))

(ert-deftest anki-gt-main-test-string-sort-yields-preorder ()
  "Alphabetical sort of full names is a pre-order traversal of the tree.
This is what `anki-gt-main--fetch-decks' relies on to place each parent
immediately before its subtree."
  (let* ((names '("Tango" "Default" "jp::active::video" "jp::active"
                  "jp" "jp::sentences"))
         (sorted (sort (copy-sequence names) #'string<)))
    (should (equal sorted
                   '("Default" "Tango" "jp" "jp::active" "jp::active::video"
                     "jp::sentences")))))

(provide 'anki-gt-test)

;;; anki-gt-test.el ends here
