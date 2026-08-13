;;; anki-gt-test.el --- Tests for anki-gt         -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT test suite for anki-gt.
;;
;; Unit tests stub `url-retrieve-synchronously' via `cl-letf' so they
;; never touch the network.  Integration tests hit a real AnkiConnect
;; and are auto-detected: they run when AnkiConnect answers a probe
;; `version' call and are skipped otherwise, so CI without a running
;; Anki stays green.  Set `ANKI_GT_NO_INTEGRATION=1' to force them off
;; even when Anki is reachable (useful for pure-offline runs).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'anki-gt)
(require 'anki-gt-main)
(require 'anki-gt-cards)
(require 'anki-gt-preview)
(require 'anki-gt-notes)

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

;;;; Integration tests (auto-detected)
;;
;; Run against a live AnkiConnect when one is reachable at
;; `anki-gt-endpoint'.  Skipped -- not failed -- when Anki is down or
;; when `ANKI_GT_NO_INTEGRATION' is set (useful for forcing pure
;; offline runs).  The reachability probe is memoised so we don't
;; hammer AnkiConnect once per test.

(defvar anki-gt-test--integration-probe-result 'unknown
  "Memoised result of the AnkiConnect reachability probe.
`unknown' means we haven't tried yet; `t' means reachable and
tests should run; `nil' means unreachable, skip.")

(defun anki-gt-test--integration-enabled-p ()
  "Return non-nil when integration tests should run.
Returns nil when `ANKI_GT_NO_INTEGRATION' is set in the
environment, or when AnkiConnect does not answer a `version'
probe.  The probe runs once per test-load and is cached."
  (unless (getenv "ANKI_GT_NO_INTEGRATION")
    (when (eq anki-gt-test--integration-probe-result 'unknown)
      (setq anki-gt-test--integration-probe-result
            (condition-case _err
                (integerp (anki-gt-request "version"))
              (error nil))))
    anki-gt-test--integration-probe-result))

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

;;;; anki-gt-cards: pure helpers

(ert-deftest anki-gt-cards-test-strip-html-plain ()
  "Plain text passes through, trimmed and whitespace-collapsed."
  (should (equal (anki-gt-cards--strip-html "  hello   world  ")
                 "hello world")))

(ert-deftest anki-gt-cards-test-strip-html-tags ()
  "HTML tags and [sound:] tags are removed; entities decoded."
  (should (equal (anki-gt-cards--strip-html
                  "<b>Hello</b> &amp; <i>bye</i>[sound:x.mp3]")
                 "Hello & bye")))

(ert-deftest anki-gt-cards-test-strip-html-nil ()
  "Nil input returns empty string, not an error."
  (should (equal (anki-gt-cards--strip-html nil) "")))

(ert-deftest anki-gt-cards-test-field-at-order-picks-by-order ()
  "`--field-at-order' returns the value of the field with the given order,
regardless of the alist's declaration order."
  (let ((card '((fields . ((Back  . ((value . "back")  (order . 1)))
                           (Front . ((value . "front") (order . 0))))))))
    (should (equal (anki-gt-cards--field-at-order card 0) "front"))
    (should (equal (anki-gt-cards--field-at-order card 1) "back"))
    ;; Missing order yields empty string, not an error.
    (should (equal (anki-gt-cards--field-at-order card 99) ""))))

(ert-deftest anki-gt-cards-test-display-field-uses-fieldOrder ()
  "`--display-field' picks the field indicated by the card's `fieldOrder'."
  (let ((card '((fieldOrder . 1)
                (fields . ((Front . ((value . "front") (order . 0)))
                           (Back  . ((value . "back")  (order . 1))))))))
    (should (equal (anki-gt-cards--display-field card) "back"))))

(ert-deftest anki-gt-cards-test-display-field-defaults-to-order-0 ()
  "`--display-field' falls back to order 0 when `fieldOrder' is absent.
This is the same behaviour we had before switching to sort-field
display, so cards from odd/legacy models still show something."
  (let ((card '((fields . ((Front . ((value . "front") (order . 0)))
                           (Back  . ((value . "back")  (order . 1))))))))
    (should (equal (anki-gt-cards--display-field card) "front"))))

(ert-deftest anki-gt-cards-test-state-label-known ()
  "Each known queue value maps to its short label."
  (should (equal (anki-gt-cards--state-label '((queue . -1))) "Susp"))
  (should (equal (anki-gt-cards--state-label '((queue . -2))) "Bury"))
  (should (equal (anki-gt-cards--state-label '((queue .  0))) "New"))
  (should (equal (anki-gt-cards--state-label '((queue .  1))) "Learn"))
  (should (equal (anki-gt-cards--state-label '((queue .  2))) "Due"))
  (should (equal (anki-gt-cards--state-label '((queue .  4))) "Prev")))

(ert-deftest anki-gt-cards-test-state-label-unknown ()
  "Unrecognised queue value falls back to \"?\"."
  (should (equal (anki-gt-cards--state-label '((queue . 99))) "?")))

(ert-deftest anki-gt-cards-test-format-interval ()
  "Positive intervals are days, negative are seconds, zero is \"0\"."
  (should (equal (anki-gt-cards--format-interval '((interval . 21))) "21d"))
  (should (equal (anki-gt-cards--format-interval '((interval . -60))) "60s"))
  (should (equal (anki-gt-cards--format-interval '((interval . 0))) "0"))
  (should (equal (anki-gt-cards--format-interval '((interval . nil))) "")))

(ert-deftest anki-gt-cards-test-template-label ()
  "Template ordinal is displayed as \"Card N\", one-based."
  (should (equal (anki-gt-cards--template-label '((ord . 0))) "Card 1"))
  (should (equal (anki-gt-cards--template-label '((ord . 2))) "Card 3"))
  ;; Missing `ord' defaults to Card 1 (never expected in practice).
  (should (equal (anki-gt-cards--template-label '()) "Card 1")))

(ert-deftest anki-gt-cards-test-entry-shape ()
  "An entry is (CARD-RECORD [DECK FIELD TEMPLATE STATE LAPSES REVIEWS IVL])."
  (let* ((card '((fields   . ((Front . ((value . "hi") (order . 0)))))
                 (ord      . 0)
                 (queue    . 2)
                 (interval . 5)
                 (lapses   . 3)
                 (reps     . 12)
                 (deckName . "Japanese::Vocab")))
         (entry (anki-gt-cards--entry card)))
    (should (eq (car entry) card))
    (should (equal (aref (cadr entry) 0) "Japanese::Vocab"))
    (should (equal (aref (cadr entry) 1) "hi"))
    (should (equal (aref (cadr entry) 2) "Card 1"))
    (should (equal (aref (cadr entry) 3) "Due"))
    (should (equal (aref (cadr entry) 4) "3"))
    (should (equal (aref (cadr entry) 5) "12"))
    (should (equal (aref (cadr entry) 6) "5d"))))

(ert-deftest anki-gt-cards-test-sort-numeric-key ()
  "`--sort-numeric-key' returns a predicate comparing by an alist value."
  (let ((sort (anki-gt-cards--sort-numeric-key 'lapses)))
    (should (funcall sort '(((lapses . 2))) '(((lapses . 10)))))
    (should-not (funcall sort '(((lapses . 10))) '(((lapses . 2)))))
    ;; Missing key is treated as 0.
    (should (funcall sort '(((foo . 1))) '(((lapses . 1)))))))

(ert-deftest anki-gt-cards-test-deck-cell-truncates ()
  "`--deck-cell' passes deckName through `anki-gt-truncate-middle'."
  (let* ((card '((deckName . "Ankidrone Starter Pack V7::1. JLPT Tango N5")))
         (anki-gt-cards-deck-width 30))
    (should (equal (anki-gt-cards--deck-cell card)
                   "Ankidrone S…::1. JLPT Tango N5"))))

;;;; anki-gt: middle-truncate helper

(ert-deftest anki-gt-test-truncate-middle-fits ()
  "Strings at or below MAX-WIDTH are returned unchanged."
  (should (equal (anki-gt-truncate-middle "abc" 10) "abc"))
  (should (equal (anki-gt-truncate-middle "exactly-10" 10) "exactly-10"))
  (should (equal (anki-gt-truncate-middle "" 5) "")))

(ert-deftest anki-gt-test-truncate-middle-no-separator ()
  "Strings without `::' fall through to a naive elide."
  (should (equal (anki-gt-truncate-middle
                  "reallyLongNameNoSeparator" 10)
                 "reallyLon…")))

(ert-deftest anki-gt-test-truncate-middle-preserves-last-segment ()
  "The trailing `…::LAST' is preserved when LAST fits in MAX-WIDTH."
  (should (equal (anki-gt-truncate-middle
                  "Ankidrone Starter Pack V7::1. JLPT Tango N5" 30)
                 "Ankidrone S…::1. JLPT Tango N5"))
  (should (equal (anki-gt-truncate-middle
                  "oldKanji::japanese::Kanji::All in one Kanji" 40)
                 "oldKanji::japanese::K…::All in one Kanji")))

(ert-deftest anki-gt-test-truncate-middle-last-too-long ()
  "When the leaf alone exceeds MAX-WIDTH, fall back to naive truncation."
  (should (equal (anki-gt-truncate-middle
                  "deck::AnAbsurdlyVeryLongLeafSegmentName" 20)
                 "deck::AnAbsurdlyVer…")))

;;;; anki-gt: literal-furigana stripper

(ert-deftest anki-gt-test-strip-literal-furigana-basic ()
  "kanji[hiragana] strips to just the kanji."
  (should (equal (anki-gt-strip-literal-furigana "具体的[ぐたいてき]")
                 "具体的"))
  (should (equal (anki-gt-strip-literal-furigana
                  "具体[ぐたい]的[てき]な例[れい]")
                 "具体的な例")))

(ert-deftest anki-gt-test-strip-literal-furigana-katakana-reading ()
  "kanji[katakana] also strips (rare but valid furigana)."
  (should (equal (anki-gt-strip-literal-furigana "犬[イヌ]") "犬")))

(ert-deftest anki-gt-test-strip-literal-furigana-preserves-annotations ()
  "Bracket content that includes ASCII survives (annotations, not furigana)."
  ;; English annotation -- keep it all
  (should (equal (anki-gt-strip-literal-furigana "犬[dog]")
                 "犬[dog]"))
  ;; MIA-style mixed marker -- English then kana -- keep it all
  (should (equal (anki-gt-strip-literal-furigana "具[toolぐ]")
                 "具[toolぐ]"))
  ;; Bracket without a kana/kanji prefix -- keep it (it's not furigana)
  (should (equal (anki-gt-strip-literal-furigana "[note]")
                 "[note]")))

(ert-deftest anki-gt-test-strip-literal-furigana-jalup-marker ()
  "Jalup / MIA format with `;marker' after the reading is stripped."
  ;; Basic Jalup: 出産[しゅっさん;h]
  (should (equal (anki-gt-strip-literal-furigana "出産[しゅっさん;h]")
                 "出産"))
  ;; Kana base (word already in kana) with a leading comma alt-form
  (should (equal (anki-gt-strip-literal-furigana "する[,する;h]")
                 "する"))
  ;; Multi-form reading with `;marker'
  (should (equal (anki-gt-strip-literal-furigana "分[わ,わかる;k2]")
                 "分"))
  ;; Empty reading, marker only (rare Jalup form)
  (should (equal (anki-gt-strip-literal-furigana "こと[;o]")
                 "こと")))

(ert-deftest anki-gt-test-strip-literal-furigana-mixed-sentence ()
  "A full Jalup sentence collapses to its plain form."
  (should (equal (anki-gt-strip-literal-furigana
                  "出産[しゅっさん;h] する[,する;h] 日[ひ;h] が 分[わ,わかる;k2]かった ？")
                 "出産 する 日 が 分かった ？")))

(ert-deftest anki-gt-test-strip-literal-furigana-nil ()
  "Nil input yields empty string."
  (should (equal (anki-gt-strip-literal-furigana nil) "")))

;;;; anki-gt: open-card-in-anki + raise

(ert-deftest anki-gt-test-open-card-in-anki-issues-expected-calls ()
  "`anki-gt-open-card-in-anki' calls guiBrowse then guiSelectCard then raise."
  (let* ((card '((cardId . 42)))
         (calls nil)
         (raised nil)
         (anki-gt-raise-anki-function (lambda () (push :raised raised))))
    (cl-letf (((symbol-function 'anki-gt-request)
               (lambda (action &optional params)
                 (push (cons action params) calls)
                 nil)))
      (anki-gt-open-card-in-anki card))
    (setq calls (nreverse calls))
    ;; Two calls in the right order.
    (should (equal (mapcar #'car calls) '("guiBrowse" "guiSelectCard")))
    ;; guiBrowse got the cid:<id> query.
    (should (equal (cdr (nth 0 calls)) '((query . "cid:42"))))
    ;; guiSelectCard got the card id.
    (should (equal (cdr (nth 1 calls)) '((card . 42))))
    ;; Raise function fired exactly once.
    (should (equal raised '(:raised)))))

(ert-deftest anki-gt-test-open-card-swallows-select-card-error ()
  "guiSelectCard's `unsupported action' on older AnkiConnect must not block
the flow: guiBrowse is still issued and the raise function still fires."
  (let* ((card '((cardId . 42)))
         (calls nil)
         (raised nil)
         (anki-gt-raise-anki-function (lambda () (setq raised t))))
    (cl-letf (((symbol-function 'anki-gt-request)
               (lambda (action &optional _params)
                 (push action calls)
                 (when (equal action "guiSelectCard")
                   (signal 'anki-gt-api-error
                           (list "unsupported action" "guiSelectCard")))
                 nil)))
      (anki-gt-open-card-in-anki card))
    (should (member "guiBrowse" calls))
    (should (member "guiSelectCard" calls))
    (should raised)))

(ert-deftest anki-gt-test-open-card-in-anki-requires-cardId ()
  "A card record missing `cardId' produces a user-error."
  (cl-letf (((symbol-function 'anki-gt-request)
             (lambda (&rest _) (error "should not be called"))))
    (should-error (anki-gt-open-card-in-anki '((foo . bar)))
                  :type 'user-error)))

(ert-deftest anki-gt-test-raise-anki-noop-when-unset ()
  "`anki-gt-raise-anki' does nothing when the function variable is nil."
  (let ((anki-gt-raise-anki-function nil))
    ;; Just make sure it doesn't error.
    (should (null (anki-gt-raise-anki)))))

(ert-deftest anki-gt-test-raise-anki-invokes-function ()
  "`anki-gt-raise-anki' calls the configured function."
  (let* ((called nil)
         (anki-gt-raise-anki-function (lambda () (setq called t))))
    (anki-gt-raise-anki)
    (should called)))

;;;; anki-gt-cards / anki-gt-notes: cross-buffer point mapping

(ert-deftest anki-gt-cards-test-goto-card-finds-existing ()
  "`--goto-card' moves point to the row whose card-id matches."
  (with-temp-buffer
    (anki-gt-cards-mode)
    (setq tabulated-list-entries
          (list (list '((cardId . 1)) ["" "" "" "" "0" "0" ""])
                (list '((cardId . 2)) ["" "" "" "" "0" "0" ""])
                (list '((cardId . 3)) ["" "" "" "" "0" "0" ""])))
    (let ((inhibit-read-only t))
      (tabulated-list-print t))
    (goto-char (point-min))
    (should (anki-gt-cards--goto-card 2))
    (should (equal (alist-get 'cardId (tabulated-list-get-id)) 2))))

(ert-deftest anki-gt-cards-test-goto-card-missing-falls-back ()
  "`--goto-card' with an unmatched id lands at point-min and returns nil."
  (with-temp-buffer
    (anki-gt-cards-mode)
    (setq tabulated-list-entries
          (list (list '((cardId . 1)) ["" "" "" "" "0" "0" ""])))
    (let ((inhibit-read-only t))
      (tabulated-list-print t))
    (should-not (anki-gt-cards--goto-card 999))
    (should (= (point) (point-min)))))

(ert-deftest anki-gt-notes-test-goto-note-finds-existing ()
  "`--goto-note' moves point to the row whose note-id matches."
  (with-temp-buffer
    (anki-gt-notes-mode)
    (setq tabulated-list-entries
          (list (list '((noteId . 10)) [""     ""      ""    ""])
                (list '((noteId . 20)) [""     ""      ""    ""])
                (list '((noteId . 30)) [""     ""      ""    ""])))
    (let ((inhibit-read-only t))
      (tabulated-list-print t))
    (goto-char (point-min))
    (should (anki-gt-notes--goto-note 30))
    (should (equal (alist-get 'noteId (tabulated-list-get-id)) 30))))

;;;; anki-gt-cards: ruby transform

(ert-deftest anki-gt-cards-test-transform-ruby-on ()
  "With `anki-gt-show-ruby' on, `<rt>X</rt>' becomes `[X]' and ruby tags drop."
  (let ((anki-gt-show-ruby t))
    (should (equal (anki-gt-cards--transform-ruby
                    "<ruby>漢字<rt>かんじ</rt></ruby>")
                   "漢字[かんじ]"))
    ;; <rb> / <rp> stripped, reading preserved.
    (should (equal (anki-gt-cards--transform-ruby
                    "<ruby><rb>漢字</rb><rp>(</rp><rt>かんじ</rt><rp>)</rp></ruby>")
                   "漢字[かんじ]"))))

(ert-deftest anki-gt-cards-test-transform-ruby-off ()
  "With `anki-gt-show-ruby' off, the reading is dropped entirely."
  (let ((anki-gt-show-ruby nil))
    (should (equal (anki-gt-cards--transform-ruby
                    "<ruby>漢字<rt>かんじ</rt></ruby>")
                   "漢字"))
    (should (equal (anki-gt-cards--transform-ruby
                    "<ruby><rb>漢字</rb><rt>かんじ</rt></ruby>")
                   "漢字"))))

(ert-deftest anki-gt-cards-test-strip-html-honors-ruby-toggle ()
  "`--strip-html' propagates the ruby toggle end-to-end."
  (let ((s "<p><ruby>犬<rt>いぬ</rt></ruby>と<ruby>猫<rt>ねこ</rt></ruby></p>"))
    (let ((anki-gt-show-ruby t))
      (should (equal (anki-gt-cards--strip-html s) "犬[いぬ]と猫[ねこ]")))
    (let ((anki-gt-show-ruby nil))
      (should (equal (anki-gt-cards--strip-html s) "犬と猫")))))

;;;; anki-gt-cards: narrow-query composer

(ert-deftest anki-gt-cards-test-narrow-query-appends ()
  "Narrow-query joins BASE and PREDICATE with a single space (implicit AND)."
  (should (equal (anki-gt-cards--narrow-query "deck:foo" "is:due")
                 "deck:foo is:due"))
  (should (equal (anki-gt-cards--narrow-query "deck:foo is:due"
                                              "tag:verb")
                 "deck:foo is:due tag:verb")))

;;;; anki-gt-preview: pure helpers + render smoke test

(ert-deftest anki-gt-preview-test-header-line-happy ()
  "Header formats card metadata using deckName and modelName, and appends
the current view mode in brackets."
  (let ((card '((cardId . 42)
                (deckName . "Japanese::Vocab")
                (modelName . "Japanese Vocab")
                (ord . 0)))
        (anki-gt-preview--view-mode 'preview))
    (should (equal (anki-gt-preview--header-line card)
                   "Card 42  •  Japanese::Vocab  •  Japanese Vocab  •  Card 1  •  [preview]"))))

(ert-deftest anki-gt-preview-test-header-line-missing-fields ()
  "Missing fields are shown as `?' or `Card 1' instead of erroring."
  (let ((card '())
        (anki-gt-preview--view-mode 'preview))
    (should (equal (anki-gt-preview--header-line card)
                   "Card ?  •  ?  •  ?  •  Card 1  •  [preview]"))))

(ert-deftest anki-gt-preview-test-html-to-dom-empty ()
  "Empty or nil HTML yields nil (no shr call)."
  (should (null (anki-gt-preview--html-to-dom nil)))
  (should (null (anki-gt-preview--html-to-dom ""))))

(ert-deftest anki-gt-preview-test-html-to-dom-parses ()
  "A minimal HTML string parses to a DOM whose root is html."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let ((dom (anki-gt-preview--html-to-dom
              "<p>hello <b>world</b></p>")))
    (should dom)
    (should (eq (car dom) 'html))))

(defun anki-gt-preview-test--ruby-buffer (html)
  "Parse HTML, render <ruby> nodes via `anki-gt-preview--render-ruby',
and return the resulting buffer text.  Non-ruby nodes are
rendered by shr's default handler.  Callers must guard on
`libxml-parse-html-region' themselves via `skip-unless'.
`shr-width' is pinned high so batch-mode line-wrapping does not
split the CJK characters we are asserting on.  `anki-gt-show-ruby'
is bound to t so these tests do not drift if the default flips."
  (with-temp-buffer
    (let ((anki-gt-show-ruby t)
          (dom (with-temp-buffer
                 (insert html)
                 (libxml-parse-html-region (point-min) (point-max))))
          (shr-width 200)
          (shr-external-rendering-functions
           (list (cons 'ruby #'anki-gt-preview--render-ruby))))
      (shr-insert-document dom))
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest anki-gt-preview-test-ruby-plain-base ()
  "Plain-text base + <rt> renders as `base[reading]'."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (should (string-match-p "漢字\\[かんじ\\]"
                          (anki-gt-preview-test--ruby-buffer
                           "<ruby>漢字<rt>かんじ</rt></ruby>"))))

(ert-deftest anki-gt-preview-test-ruby-explicit-rb ()
  "<rb>base</rb> + <rt> renders the same as bare base + <rt>."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (should (string-match-p "漢字\\[かんじ\\]"
                          (anki-gt-preview-test--ruby-buffer
                           "<ruby><rb>漢字</rb><rt>かんじ</rt></ruby>"))))

(ert-deftest anki-gt-preview-test-ruby-multi-segment ()
  "Interleaved base + rt pairs render as separate `base[reading]' spans."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (should (string-match-p "漢\\[かん\\]字\\[じ\\]"
                          (anki-gt-preview-test--ruby-buffer
                           "<ruby>漢<rt>かん</rt>字<rt>じ</rt></ruby>"))))

(ert-deftest anki-gt-preview-test-ruby-rp-dropped ()
  "<rp> is dropped since our brackets already delimit the reading."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let ((s (anki-gt-preview-test--ruby-buffer
            "<ruby>漢字<rp>(</rp><rt>かんじ</rt><rp>)</rp></ruby>")))
    ;; Result should be `漢字[かんじ]', no stray parens.
    (should (string-match-p "漢字\\[かんじ\\]" s))
    (should-not (string-match-p "(" s))))

(ert-deftest anki-gt-preview-test-ruby-off-drops-reading ()
  "With `anki-gt-show-ruby' off, `<rt>...</rt>' is not emitted."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (with-temp-buffer
    (let ((anki-gt-show-ruby nil)
          (dom (with-temp-buffer
                 (insert "<ruby>漢字<rt>かんじ</rt></ruby>")
                 (libxml-parse-html-region (point-min) (point-max))))
          (shr-width 200)
          (shr-external-rendering-functions
           (list (cons 'ruby #'anki-gt-preview--render-ruby))))
      (shr-insert-document dom))
    (let ((out (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "漢字" out))
      (should-not (string-match-p "かんじ" out))
      (should-not (string-match-p "\\[" out)))))

(ert-deftest anki-gt-preview-test-ruby-face-on-reading ()
  "The reading span carries `anki-gt-preview-furigana-face'."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (with-temp-buffer
    (let ((dom (with-temp-buffer
                 (insert "<ruby>漢字<rt>かんじ</rt></ruby>")
                 (libxml-parse-html-region (point-min) (point-max))))
          (shr-width 200)
          (shr-external-rendering-functions
           (list (cons 'ruby #'anki-gt-preview--render-ruby))))
      (shr-insert-document dom))
    (goto-char (point-min))
    (search-forward "[")
    (should (eq (get-text-property (1- (point)) 'face)
                'anki-gt-preview-furigana-face))))

;;;; anki-gt-preview: media rewriting

(ert-deftest anki-gt-preview-test-rewrite-img-bare-filename ()
  "<img src=\"foo.jpg\"> is rewritten to a file:// URL under DIR."
  (should (equal (anki-gt-preview--rewrite-media
                  "<img src=\"foo.jpg\">" "/tmp/media")
                 "<img src=\"file:///tmp/media/foo.jpg\">"))
  ;; Single-quoted attribute also works.
  (should (equal (anki-gt-preview--rewrite-media
                  "<img src='foo.jpg'>" "/tmp/media")
                 "<img src='file:///tmp/media/foo.jpg'>")))

(ert-deftest anki-gt-preview-test-rewrite-img-with-attrs ()
  "Other <img> attributes before or after src survive the rewrite."
  (should (equal (anki-gt-preview--rewrite-media
                  "<img class=\"pic\" src=\"foo.jpg\" alt=\"x\">"
                  "/tmp/media")
                 "<img class=\"pic\" src=\"file:///tmp/media/foo.jpg\" alt=\"x\">")))

(ert-deftest anki-gt-preview-test-rewrite-img-skips-absolute ()
  "<img src=\"/abs/path\"> is left alone."
  (should (equal (anki-gt-preview--rewrite-media
                  "<img src=\"/abs/pic.jpg\">" "/tmp/media")
                 "<img src=\"/abs/pic.jpg\">")))

(ert-deftest anki-gt-preview-test-rewrite-img-skips-url ()
  "<img src=\"https://...\"> and any other scheme is left alone."
  (should (equal (anki-gt-preview--rewrite-media
                  "<img src=\"https://example.com/pic.jpg\">"
                  "/tmp/media")
                 "<img src=\"https://example.com/pic.jpg\">"))
  (should (equal (anki-gt-preview--rewrite-media
                  "<img src=\"data:image/png;base64,ABCD\">"
                  "/tmp/media")
                 "<img src=\"data:image/png;base64,ABCD\">")))

(ert-deftest anki-gt-preview-test-rewrite-img-encodes-spaces ()
  "Spaces in the resolved file path are percent-encoded."
  (should (equal (anki-gt-preview--rewrite-media
                  "<img src=\"a b.jpg\">" "/tmp/media")
                 "<img src=\"file:///tmp/media/a%20b.jpg\">")))

(ert-deftest anki-gt-preview-test-rewrite-sound-tag ()
  "[sound:foo.mp3] becomes an <a href=\"anki-audio:foo.mp3\">."
  (should (equal (anki-gt-preview--rewrite-media
                  "[sound:foo.mp3]" "/tmp/media")
                 "<a href=\"anki-audio:foo.mp3\">[▶ foo.mp3]</a>")))

(ert-deftest anki-gt-preview-test-rewrite-sound-tag-encodes-unusual-names ()
  "Filenames with spaces or unicode are URL-hexified in the href."
  (let ((out (anki-gt-preview--rewrite-media
              "[sound:hello world.mp3]" "/tmp/media")))
    ;; Space is encoded in the href, visible label preserves the space.
    (should (string-match-p "href=\"anki-audio:hello%20world\\.mp3\""
                            out))
    (should (string-match-p "\\[▶ hello world\\.mp3\\]" out))))

(ert-deftest anki-gt-preview-test-rewrite-nil-or-empty ()
  "Nil or empty input returns an empty (or unchanged) string."
  (should (equal (anki-gt-preview--rewrite-media nil "/tmp/media") ""))
  (should (equal (anki-gt-preview--rewrite-media "" "/tmp/media") "")))

(ert-deftest anki-gt-preview-test-rewrite-multiple-refs ()
  "Multiple img and sound references are all rewritten in one pass."
  (let ((out (anki-gt-preview--rewrite-media
              "<img src=\"a.jpg\"> and <img src=\"b.png\"> [sound:x.mp3]"
              "/tmp/media")))
    (should (string-match-p "file:///tmp/media/a\\.jpg" out))
    (should (string-match-p "file:///tmp/media/b\\.png" out))
    (should (string-match-p "anki-audio:x\\.mp3" out))))

;;;; anki-gt-preview: minimal CSS parser + application

(ert-deftest anki-gt-preview-test-css-strip-comments ()
  "CSS comments are removed before parsing."
  (should (equal (anki-gt-preview--strip-css-comments
                  "/* c */.a { color: red } /* trailing */ ")
                 ".a { color: red }  ")))

(ert-deftest anki-gt-preview-test-css-parse-simple ()
  "A single class rule with two supported properties is parsed."
  (let ((rules (anki-gt-preview--parse-css
                ".big { font-size: 300%; color: red; }")))
    (should (equal rules
                   '((:selectors (".big")
                      :props (("font-size" . "300%") ("color" . "red"))))))))

(ert-deftest anki-gt-preview-test-css-parse-drops-unsupported ()
  "Rules touching only unsupported props are dropped entirely."
  (should (null (anki-gt-preview--parse-css
                 ".a { text-align: center; font-family: Arial; }"))))

(ert-deftest anki-gt-preview-test-css-parse-comma-separated-selectors ()
  "A comma-separated selector list becomes a list of strings."
  (let ((rules (anki-gt-preview--parse-css
                ".a, .b, .c { color: red; }")))
    (should (equal (plist-get (car rules) :selectors)
                   '(".a" ".b" ".c")))))

(ert-deftest anki-gt-preview-test-css-parse-skips-at-rules-top-level ()
  "A top-level at-rule with a supported body is dropped.
Documents that we recognise `@...' at the start of a selector.
Nested rules inside `@media' still leak through because our
regex-based parser doesn't track brace nesting; that gap is
acceptable for Anki templates, which don't use `@media'."
  ;; Top-level @-rule with no nested rule is dropped entirely.
  (should (null (anki-gt-preview--parse-css
                 "@font-face { font-family: X; font-size: 10px; }"))))

(ert-deftest anki-gt-preview-test-css-selector-matches-class ()
  "`.big' matches a DOM node whose class attribute contains `big'."
  (should (anki-gt-preview--selector-matches-p
           ".big" '(div ((class . "big warn")) "hi")))
  (should-not (anki-gt-preview--selector-matches-p
               ".big" '(div ((class . "small")) "hi"))))

(ert-deftest anki-gt-preview-test-css-selector-matches-id ()
  "`#idname' matches when the DOM node's id attribute equals `idname'."
  (should (anki-gt-preview--selector-matches-p
           "#top" '(div ((id . "top")) "hi")))
  (should-not (anki-gt-preview--selector-matches-p
               "#top" '(div ((id . "bottom")) "hi"))))

(ert-deftest anki-gt-preview-test-css-selector-matches-tag ()
  "A bare tag selector matches DOM nodes with that tag."
  (should (anki-gt-preview--selector-matches-p
           "div" '(div nil "hi")))
  (should-not (anki-gt-preview--selector-matches-p
               "div" '(span nil "hi"))))

(ert-deftest anki-gt-preview-test-css-selector-matches-tag-class ()
  "`tag.class' requires both to match."
  (should (anki-gt-preview--selector-matches-p
           "div.big" '(div ((class . "big")) "hi")))
  (should-not (anki-gt-preview--selector-matches-p
               "div.big" '(span ((class . "big")) "hi")))
  (should-not (anki-gt-preview--selector-matches-p
               "div.big" '(div ((class . "small")) "hi"))))

(ert-deftest anki-gt-preview-test-css-font-size-percent ()
  (should (equal (anki-gt-preview--css-font-size-height "200%") 2.0))
  (should (equal (anki-gt-preview--css-font-size-height "50%") 0.5)))

(ert-deftest anki-gt-preview-test-css-font-size-em ()
  (should (equal (anki-gt-preview--css-font-size-height "1.5em") 1.5))
  (should (equal (anki-gt-preview--css-font-size-height "2rem") 2.0)))

(ert-deftest anki-gt-preview-test-css-font-size-px ()
  "px is normalised against a 16-px baseline."
  (should (equal (anki-gt-preview--css-font-size-height "32px") 2.0))
  (should (equal (anki-gt-preview--css-font-size-height "8px") 0.5)))

(ert-deftest anki-gt-preview-test-css-font-size-pt ()
  "pt becomes an integer in 1/10 pt (Emacs' absolute `:height' scale)."
  (should (equal (anki-gt-preview--css-font-size-height "20pt") 200)))

(ert-deftest anki-gt-preview-test-css-font-size-invalid ()
  (should (null (anki-gt-preview--css-font-size-height "larger")))
  (should (null (anki-gt-preview--css-font-size-height "xxl"))))

(ert-deftest anki-gt-preview-test-css-props-to-face ()
  "Multiple CSS properties compose into a face plist."
  (let ((face (anki-gt-preview--props-to-face
               '(("font-size" . "200%")
                 ("color" . "red")
                 ("background-color" . "yellow")
                 ("font-weight" . "bold")))))
    (should (equal (plist-get face :height) 2.0))
    (should (equal (plist-get face :foreground) "red"))
    (should (equal (plist-get face :background) "yellow"))
    (should (eq (plist-get face :weight) 'bold))))

(ert-deftest anki-gt-preview-test-css-props-to-face-empty ()
  "No supported properties produces nil, not an empty plist."
  (should (null (anki-gt-preview--props-to-face
                 '(("text-align" . "center"))))))

(ert-deftest anki-gt-preview-test-css-face-for-node ()
  "Face computation walks all rules; later rules override earlier ones."
  (let ((anki-gt-preview--css-rules
         '((:selectors (".big")
            :props (("font-size" . "200%") ("color" . "red")))
           (:selectors (".big")
            :props (("color" . "blue"))))))
    (let ((face (anki-gt-preview--face-for-node
                 '(div ((class . "big")) "hi"))))
      (should (equal (plist-get face :height) 2.0))
      ;; Later rule wins for color.
      (should (equal (plist-get face :foreground) "blue")))))

;;;; anki-gt-preview: field-level audio extraction

(ert-deftest anki-gt-preview-test-sound-refs-none ()
  "A string with no [sound:X] returns nil, not the empty list."
  (should (null (anki-gt-preview--sound-refs-in-string "no audio here")))
  (should (null (anki-gt-preview--sound-refs-in-string "")))
  (should (null (anki-gt-preview--sound-refs-in-string nil))))

(ert-deftest anki-gt-preview-test-sound-refs-multiple ()
  "Multiple [sound:X] occurrences are returned in source order."
  (should (equal (anki-gt-preview--sound-refs-in-string
                  "hello [sound:a.mp3] world [sound:b.mp3]!")
                 '("a.mp3" "b.mp3"))))

(ert-deftest anki-gt-preview-test-card-audio-refs-dedup ()
  "Audio refs across fields are de-duplicated by filename.
Each result is (FIELD-NAME . FILENAME); the first occurrence's
field name is retained."
  (let ((card `((fields . ((Word     . ((value . "[sound:a.mp3]") (order . 0)))
                           (Sentence . ((value . "s [sound:b.mp3]") (order . 1)))
                           (Extra    . ((value . "[sound:a.mp3]") (order . 2))))))))
    (should (equal (anki-gt-preview--card-audio-refs card)
                   '((Word . "a.mp3") (Sentence . "b.mp3"))))))

(ert-deftest anki-gt-preview-test-card-audio-refs-empty ()
  "A card with no [sound:X] anywhere returns nil."
  (let ((card '((fields . ((Front . ((value . "hello") (order . 0)))
                           (Back  . ((value . "world") (order . 1))))))))
    (should (null (anki-gt-preview--card-audio-refs card)))))

;;;; anki-gt-preview: fields view

(ert-deftest anki-gt-preview-test-insert-fields-sorts-by-order ()
  "Fields are emitted in order-value sequence, not alist declaration order."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let ((card '((fields . ((Back  . ((value . "back")   (order . 1)))
                           (Front . ((value . "front")  (order . 0)))
                           (Extra . ((value . "extra")  (order . 2))))))))
    (with-temp-buffer
      (anki-gt-preview-mode)
      (let ((inhibit-read-only t)
            (anki-gt-preview--view-mode 'fields)
            (anki-gt-preview--record card))
        (anki-gt-preview--insert-fields card))
      (let ((positions (mapcar (lambda (label)
                                 (goto-char (point-min))
                                 (search-forward label nil t))
                               '("Front" "Back" "Extra"))))
        ;; Every label found and in strictly increasing position.
        (should (cl-every #'identity positions))
        (should (apply #'< positions))))))

(ert-deftest anki-gt-preview-test-insert-fields-marks-empty ()
  "Empty field values render as an `(empty)' hint in the shadow face."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (let ((card '((fields . ((F . ((value . "") (order . 0))))))))
    (with-temp-buffer
      (anki-gt-preview-mode)
      (let ((inhibit-read-only t)
            (anki-gt-preview--view-mode 'fields))
        (anki-gt-preview--insert-fields card))
      (goto-char (point-min))
      (should (search-forward "(empty)" nil t)))))

(ert-deftest anki-gt-preview-test-toggle-view-flips ()
  "`anki-gt-preview-toggle-view' cycles between preview and fields."
  (let ((card '((cardId . 1) (fields . ((F . ((value . "hi") (order . 0)))))
                (answer . "<p>hi</p>"))))
    (with-temp-buffer
      (anki-gt-preview-mode)
      (setq-local anki-gt-preview--record card)
      (setq anki-gt-preview--view-mode 'preview)
      (anki-gt-preview-toggle-view)
      (should (eq anki-gt-preview--view-mode 'fields))
      (anki-gt-preview-toggle-view)
      (should (eq anki-gt-preview--view-mode 'preview)))))

(ert-deftest anki-gt-preview-test-header-line-shows-view-mode ()
  "The metadata header appends the current view state in brackets."
  (let ((card '((cardId . 42) (deckName . "D") (modelName . "M") (ord . 0))))
    (let ((anki-gt-preview--view-mode 'preview))
      (should (string-suffix-p "[preview]"
                               (anki-gt-preview--header-line card))))
    (let ((anki-gt-preview--view-mode 'fields))
      (should (string-suffix-p "[fields]"
                               (anki-gt-preview--header-line card))))))

(ert-deftest anki-gt-preview-test-render-smoke ()
  "Render a minimal card into a fresh buffer without erroring.
Not asserting content, just that the code path runs and leaves
the buffer non-empty with the card cached."
  (let ((card '((cardId . 1)
                (deckName . "D")
                (modelName . "M")
                (ord . 0)
                (question . "<p>Q</p>")
                (answer . "<p>Q</p><hr id=\"answer\"><p>A</p>"))))
    (with-temp-buffer
      (anki-gt-preview-mode)
      (anki-gt-preview--render card)
      (should (> (buffer-size) 0))
      (should (eq anki-gt-preview--record card)))))

(provide 'anki-gt-test)

;;; anki-gt-test.el ends here
