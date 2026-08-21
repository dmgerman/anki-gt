# Makefile for anki-gt.
#
# Package-specific settings only; every rule lives in Makefile.common,
# which is an identical copy across the dmg packages.  Run `make help'
# for the target list, and see the header of Makefile.common for what
# each variable below controls.
#
# Integration tests that hit a live AnkiConnect are auto-detected: they
# run when AnkiConnect answers a `version' probe and are skipped
# otherwise, so `make test' works whether Anki is running or not.  Set
# ANKI_GT_NO_INTEGRATION=1 to force them off even with Anki up.

PACKAGE = anki-gt

# Foundational file first so follow-on files can (require 'anki-gt)
# without erroring when compiled in isolation.
EL_FILES = anki-gt.el \
           anki-gt-main.el \
           anki-gt-cards.el \
           anki-gt-notes.el \
           anki-gt-preview.el

DEPS = package-lint

TEST_FILES = test/anki-gt-test.el

INFO_SRC = README.org

# Org's texinfo exporter leaves this behind when export fails partway.
EXTRA_CLEAN = README.texi

include Makefile.common
