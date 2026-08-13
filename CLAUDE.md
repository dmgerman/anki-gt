# anki-gt — agent notes

Emacs client for the AnkiConnect HTTP API.  Plain multi-file Elisp
package (not literate).  Additional design and reference notes live
under `ai/`.

## Files

- `anki-gt.el` — transport (`anki-gt-request`), shared helpers, top-level `anki-gt` command.
- `anki-gt-main.el` — decks buffer (`*anki-gt*`).
- `anki-gt-cards.el` — cards list (`*anki-gt: DECK*`).
- `anki-gt-notes.el` — notes list (`*anki-gt-notes: DECK*`).
- `anki-gt-preview.el` — preview pane (`*anki-gt-preview*`, TAB toggles rendered card ↔ fields).

## Development

- `make check` — compile + lint + checkdoc + check-declare + tests.
- `make test` — tests only.  Integration tests probe a running Anki
  and self-skip when it is not reachable.  `ANKI_GT_NO_INTEGRATION=1`
  forces them off unconditionally.
- Do NOT `git commit`.  The user runs commits themselves.

## Gotchas that have bit us

- **Match data**: `url-encode-url`, `url-hexify-string`, `string-trim`
  run internal regexes that clobber the global match data.  Inside a
  `while string-match` loop, capture every `match-string` and
  `match-end` BEFORE calling any helper — otherwise `replace-match`
  uses stale positions and you get corrupted output or an infinite
  loop.
- **`defvar` short-circuits on reload**.  A mode-map defined as
  `(defvar map (let ...))` will not pick up new bindings without an
  Emacs restart.  Define the map as an empty `defvar` and set keys in
  a plain `let`/`define-key` block outside that re-runs on every load.
- **`load-prefer-newer` is nil by default**.  The Makefile sets it to
  `t` in `EMACS_BATCH` so tests always see fresh sources.  Ad-hoc
  `emacs -Q --batch -l …` invocations need the same setting or they
  load stale `.elc` files.
- **Media dir is huge** — never `ls DIR/` or `directory-files` it.
  Use narrow globs or AnkiConnect's `getMediaFilesNames`.
- **AnkiConnect quirks**: `getDeckStats` takes deck NAMES (not IDs
  despite returning objects keyed by id); `guiSelectCard` errors on
  older installs and we swallow that; `notesInfo` does not return
  `fieldOrder` (we default to the field with order 0).
- **shr has no CSS engine** for class / id rules.  We parse a minimal
  subset in `anki-gt-preview--parse-css` (font-size, color,
  background-color, font-weight, `.class`/`#id`/`tag`/`tag.class`).
  Ruby renders inline as `base[reading]` because Emacs cannot stack
  furigana above kanji.

## `ai/` contents

- `feature.org` — full AnkiConnect action reference (extracted from upstream README).
- `card-preview.md` — spec for a `guiPreviewCard` AnkiConnect action we would like but do not yet have.
