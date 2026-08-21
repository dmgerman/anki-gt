# Top-level Makefile for anki-gt.
#
# Drives the elisp side: compile, lint, checkdoc, check-declare, and
# the ERT test suite.
#
# Targets:
#   make                — compile (default)
#   make lint           — package-lint every anki-gt*.el file
#   make checkdoc       — checkdoc every anki-gt*.el file (errors on any warning)
#   make check-declare  — verify declare-function file arguments (errors on any mismatch)
#   make compile        — byte-compile every anki-gt*.el file (errors on warning)
#   make test           — run the ERT suite in test/ (batch)
#   make info           — export README.org to docs/anki-gt.info
#   make clean          — remove every *.elc file and generated docs
#   make check          — compile + lint + checkdoc + check-declare + test
#   make check-ci       — `make check' under every Emacs in
#                         $(CI_EMACS_LIST) (emacs-plus@30 and @31,
#                         matching the GitHub Actions matrix; run
#                         before pushing).  Errors out when either
#                         binary is absent.
#
# Override the Emacs binary by passing EMACS=path/to/emacs.
#
# Integration tests that hit a live AnkiConnect are auto-detected:
# they run when AnkiConnect answers a `version' probe and are skipped
# otherwise, so `make test' works whether Anki is running or not.  To
# force them off (e.g. for pure offline / CI-clean runs even with Anki
# up), set `ANKI_GT_NO_INTEGRATION=1' in the environment.

EMACS ?= emacs

# Foundational files first so follow-on files can (require 'anki-gt) without
# erroring when compiled in isolation.
EL_FILES = anki-gt.el \
           anki-gt-main.el \
           anki-gt-cards.el \
           anki-gt-notes.el \
           anki-gt-preview.el

TEST_FILES = test/anki-gt-test.el

# Project-local ELPA so the user's personal package directory is not touched
# and CI starts from a clean slate every run.
ELPA_DIR = .elpa

# Dependencies installed into the project-local ELPA before lint/compile.
# `package-lint' is the lint tool itself.  anki-gt has no runtime
# dependencies beyond Emacs 27+ (url and json are built in), so nothing
# else lives here yet.
DEPS = package-lint

# Common Emacs invocation header: project-local package-user-dir, MELPA in
# package-archives, package-initialize so installed packages are on load-path.
# `load-prefer-newer' forces `require' to pick whichever of .el / .elc is
# newer, so a stale .elc from a previous `make compile' can never mask a
# fresh source edit during `make test' or any ad-hoc load.  This makes
# `rm *.elc' unnecessary as a manual pre-step.
EMACS_BATCH = $(EMACS) -Q --batch \
  --eval "(setq load-prefer-newer t)" \
  --eval "(setq package-user-dir (expand-file-name \"$(ELPA_DIR)\"))" \
  --eval "(require 'package)" \
  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
  --eval "(package-initialize)"

INFO_FILE = anki-gt.info
INFO_DIR  = dir

.PHONY: default lint checkdoc check-declare compile test info clean check check-ci

# Default target: byte-compile the elisp.  Lint and tests are not
# included so the common edit-then-`make' loop stays fast; run
# `make check' before committing.
default: compile

$(ELPA_DIR):
	@mkdir -p $@

$(ELPA_DIR)/.installed: | $(ELPA_DIR)
	$(EMACS_BATCH) \
	  --eval "(unless package-archive-contents (package-refresh-contents))" \
	  $(foreach pkg,$(DEPS),--eval "(unless (package-installed-p '$(pkg)) (package-install '$(pkg)))")
	@touch $@

lint: $(ELPA_DIR)/.installed
	$(EMACS_BATCH) \
	  --eval "(require 'package-lint)" \
	  -f package-lint-batch-and-exit $(EL_FILES)

# checkdoc runs in batch via `checkdoc-file', which writes warnings to
# stderr (via `display-warning') but never exits non-zero on its own.
# After each file, peek at the `*Warnings*' buffer to detect whether any
# warning was emitted and exit 1 on the first one so CI fails on
# regressions.  Stderr already carries the human-readable diagnostic;
# no need to re-print it.  `-L .' lets each file `require' its siblings
# during checkdoc's own load.
checkdoc:
	@$(EMACS_BATCH) \
	  -L . \
	  --eval "(require 'checkdoc)" \
	  --eval "(let ((had-issue nil)) \
	            (dolist (f command-line-args-left) \
	              (with-current-buffer (get-buffer-create \"*Warnings*\") (erase-buffer)) \
	              (checkdoc-file f) \
	              (when (> (buffer-size (get-buffer-create \"*Warnings*\")) 0) \
	                (setq had-issue t))) \
	            (when had-issue (kill-emacs 1)))" \
	  $(EL_FILES)

# check-declare verifies the file argument of every `declare-function' form
# by loading the named file and checking that the function is defined there.
# `check-declare-file' returns a list of errors (or nil on success) and
# writes a human-readable report to the `*Check Declarations Warnings*'
# buffer.  We aggregate over all files and exit 1 on any finding so CI
# fails on regressions.  `-L .' lets each file `require' its siblings.
check-declare:
	@$(EMACS_BATCH) \
	  -L . \
	  --eval "(require 'check-declare)" \
	  --eval "(let ((had-issue nil)) \
	            (dolist (f command-line-args-left) \
	              (when (check-declare-file f) \
	                (setq had-issue t))) \
	            (when had-issue \
	              (with-current-buffer (get-buffer-create check-declare-warning-buffer) \
	                (princ (buffer-string))) \
	              (kill-emacs 1)))" \
	  $(EL_FILES)

# Compile each file in a fresh subprocess so a definition leaked by one file
# cannot mask a missing `require' in another.  Treats every byte-compile
# warning as a hard error so CI catches them before commit.  `-L .' puts the
# source tree on the load-path so files compile in order even though they
# (require 'anki-gt) before anki-gt.elc exists.
compile: $(ELPA_DIR)/.installed
	@set -e; \
	for f in $(EL_FILES); do \
	  echo "==> compiling $$f"; \
	  $(EMACS_BATCH) \
	    --eval "(setq byte-compile-error-on-warn t)" \
	    -L . \
	    -f batch-byte-compile $$f; \
	done

# Run the ERT suite in batch.  `-L .' puts the sources on the load-path
# so tests can (require 'anki-gt).  Integration tests that hit a live
# AnkiConnect auto-detect reachability via a `version' probe; they run
# when Anki is up and self-skip otherwise, so this target is safe in
# CI without a running Anki.  Set `ANKI_GT_NO_INTEGRATION=1' to force
# them off unconditionally.
test:
	$(EMACS_BATCH) \
	  -L . \
	  $(foreach f,$(TEST_FILES),-l $(f)) \
	  -f ert-run-tests-batch-and-exit

# Build the Info manual and its `dir' entry alongside the sources.
# Follows the multi-file ELPA convention: `.info' and `dir' live at
# the package's root so `package.el' auto-registers them via
# `Info-directory-list' on activation.  Both files are checked in.
#
# Requires `makeinfo' and `install-info' in PATH.  Uses Org's
# texinfo exporter for the .info; `install-info' produces `dir'
# from the `INFO-DIR-*' fields at the top of README.org.
info: $(INFO_FILE) $(INFO_DIR)

$(INFO_FILE): README.org
	@# Stage README.org as anki-gt.org so Org's basename-derived
	@# output aligns with the `#+TEXINFO_FILENAME' inside; otherwise
	@# Org compiles README.texi and then can't find README.info.
	cp README.org anki-gt.org
	$(EMACS) -Q --batch \
	  --eval "(setq load-prefer-newer t)" \
	  --eval "(require 'ox-texinfo)" \
	  anki-gt.org \
	  -f org-texinfo-export-to-info
	@rm -f anki-gt.org anki-gt.texi

$(INFO_DIR): $(INFO_FILE)
	install-info --info-file=$(INFO_FILE) --dir-file=$(INFO_DIR)

# `clean' does NOT remove $(INFO_FILE) or $(INFO_DIR): both are
# committed artifacts consumed by the ELPA package activation.
# Rebuild them with `make info' when README.org changes.
clean:
	rm -f *.elc test/*.elc README.texi

check: compile lint checkdoc check-declare test

# CI-mirror check.  EMACS_30 / EMACS_31 are the Package-Requires floor
# and the latest release, matching the GitHub Actions matrix in
# .github/workflows/package-lint.yml.  Both are mandatory: a skipped
# version reports a pass that CI does not agree with, so `check-ci'
# refuses to run until both are installed.  The default `make check'
# runs under whatever `emacs' resolves to on PATH and cannot prove
# multi-version compatibility.
EMACS_30 ?= /opt/homebrew/opt/emacs-plus@30/bin/emacs
EMACS_31 ?= /opt/homebrew/opt/emacs-plus@31/bin/emacs
CI_EMACS_LIST ?= $(EMACS_30) $(EMACS_31)

# Every binary is verified before the first one runs, so a missing
# install is reported up front rather than after a full pass.
check-ci:
	@missing=""; \
	for e in $(CI_EMACS_LIST); do \
	  [ -x "$$e" ] || missing="$$missing $$e"; \
	done; \
	if [ -n "$$missing" ]; then \
	  echo "check-ci: required Emacs not executable:$$missing"; \
	  echo "Install both:  brew install emacs-plus@30 emacs-plus@31"; \
	  echo "Or override:   make check-ci CI_EMACS_LIST=\"/path/to/emacs ...\""; \
	  exit 1; \
	fi
	@for e in $(CI_EMACS_LIST); do \
	  echo "==> check-ci under $$e ($$($$e --version | head -1))"; \
	  $(MAKE) EMACS=$$e check || exit 1; \
	done
