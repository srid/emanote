# Issue #66 — High memory usage on large notebooks

[Issue #66](https://github.com/srid/emanote/issues/66) reports that a
notebook with 4561 markdown files (69 MiB on disk) makes Emanote consume
**~4.7 GiB** of resident memory at steady state.

This PR is **investigation infrastructure plus an honest negative
result** on the architectural attempt: a checked-in reproducer, a
reusable measurement harness, baseline numbers, full closure-type
attribution, and the root-cause hypothesis the next PR can attack
directly. The architectural fix itself didn't land — every attempt
either broke even or regressed RSS at the issue's scale, and the
profile shows the leak lives somewhere I haven't isolated yet.

## What's in this PR

- [`scripts/gen-fixture.py`](../scripts/gen-fixture.py) — generates a
  4500-note synthetic notebook matching the issue's shape
  (frontmatter, ~30 lorem paragraphs, 4-8 wikilinks per note, inline
  hashtags, external link, blockquote with bold/italic, code span).
  ~55 MiB on disk; close to the issue's per-note density.
- [`scripts/measure-load-rss.sh`](../scripts/measure-load-rss.sh) —
  launches `emanote run`, polls `VmRSS` until it plateaus
  (settle-detection — `unionMount` opens the listening port long
  before all 4500 files are loaded, so a fixed sleep would
  under-sample), then reports `VmHWM` from `/proc/<pid>/status`.
- This document — the running log of the investigation.

```sh
nix run nixpkgs#python3 -- ./scripts/gen-fixture.py /tmp/emanote-fixture --count 4500
nix build --no-link --print-out-paths .#default
BIN=$(nix build --no-link --print-out-paths .#default)/bin/emanote
./scripts/measure-load-rss.sh "$BIN" /tmp/emanote-fixture --label master
```

## Baseline measurements

Three-run median post-load `VmHWM`:

| Fixture                     | On disk    | Master peak RSS |
| --------------------------- | ---------: | --------------: |
| 4500 notes × 5 KiB body     |   24 MiB   |    **1268 MiB** |
| 4500 notes × 15 KiB body    |   55 MiB   |    **2420 MiB** |

The issue's reported 4.7 GiB on a 4561 × 15 KiB notebook lines up with
the 2.4 GiB measurement here once you account for the extra notes and
(likely) richer markdown — the curve scales linearly with note count
and roughly linearly with per-note size.

A single static-`gen` run on the smaller fixture also serves as a
peak-load + render check:

```
allocated in the heap          : 2.86 TiB
maximum residency              : 523 MiB
total memory in use            : 1581 MiB
Gen 0 collections              : 717,374
Gen 1 collections              : 308 (173s in major GC)
Productivity                   : 63.1% user, 81.4% elapsed
Wall                           : 12:39
Maximum resident set size      : 1.60 GiB
```

37% of CPU goes to GC. The model retains too much.

## Heap profile attribution (master, issue-scale, last sample at ~47s)

`+RTS -hT` produces a closure-type heap profile without requiring a
`-prof`-enabled GHC build (which under Nix would mean rebuilding every
dependency). Top retainers, sorted by bytes, on the issue-scale
fixture:

| Closure                                          | Size (B)      |
| ------------------------------------------------ | ------------: |
| `text:Data.Text.Internal.Text`                   |   278,330,624 |
| `Sequence.Internal.Node2`                        |   213,949,984 |
| `Sequence.Internal.Node3`                        |   178,518,000 |
| `Pandoc.Definition.Str`                          |   138,340,944 |
| `ARR_WORDS`                                      |    84,133,016 |
| `THUNK`                                          |    27,659,384 |
| `Sequence.Deep`                                  |    17,464,680 |
| `THUNK_2_0`                                      |    12,615,328 |
| `GHC.Types.:` (cons cells)                       |    10,141,272 |
| `Sequence.One` / `Three` / `Two`                 |    21.5 MiB combined |
| `(,)` / `(,,)` tuples                            |     4.3 MiB combined |
| _(everything below 4 MiB elided)_                |               |

Roughly **~1.0 GiB live heap**, of which ~830 MiB is the parsed
**`Pandoc` tree** pinned per-note in `_modelNotes`
(`Sequence.Internal.Node*` finger-tree spine + `Pandoc.Str` cells +
the `Text` headers backing each `Str` + the `ARR_WORDS` backing each
`Text`). The remaining ~1.4 GiB of RSS is fixed: GHC's heap-block
reserve, executable text, statically linked libraries, the Lua
scripting engine, Heist template state, RTS arenas — none of it
sensitive to the note count.

`THUNK / THUNK_*` adds up to ~48 MiB. lua-vr's organon-style
`evaluate . force` after parse would reclaim most of that — about 5%
of the model heap. Real but not enough.

## Hypotheses → architectural attempt → result

The profile makes the lever obvious: **the parsed `Pandoc` tree is
being retained in `_modelNotes` for every note, and forcing thunks or
shrinking individual records won't help much because the *structure*
itself is the cost**. This PR explored four stacked architectural
moves that should, on paper, take all of that retention out:

1. **`NoteContent` ADT** in `Emanote.Model.Note`. Replace
   `_noteDoc :: Pandoc` with `NoteContentMd Text | NoteContentOrg Text
   | NoteContentParsed Pandoc`. The model holds raw source text for
   the common case (no Lua/Pandoc filters declared in the note's
   frontmatter) and re-parses on render via a `noteDoc :: Note ->
   Pandoc` getter. Re-parse is pure (`Markdown.parseMarkdown` +
   `preparePandoc`), so the renderer stays in pure code. For the rare
   filter-using path the original parsed Pandoc is cached.

2. **`_relCtx :: !Text`** in `Emanote.Model.Link.Rel` (was
   `[B.Block]`). Plain-text rendering of the surrounding block context
   computed once via `WL.plainify . W.query inlinesOf` + `T.copy` so
   the rendered text doesn't keep a slice of the source's
   `ARR_WORDS`. `Graph.modelLookupBacklinks` and
   `View.Template.backlinksSplice` consume `Text` and wrap it in a
   single `B.Plain` block to satisfy the existing template contract.

3. **Pre-extracted `IxRel` / `IxTask`** at handler-call time
   (`modelInsertNoteIndexed` / `modelInsertNoteWithDoc` triple). Force
   `rels` and `tasks` to WHNF *before* constructing the returned
   `model -> model` closure, so the closure body references
   `(note, rels, tasks)` instead of `(note, doc)` — the parsed Pandoc
   becomes GC-eligible as soon as the loader moves on.

4. **`unionMountStreaming`** upstream in `srid/unionmount` plus
   `streamFsChanges` in `Emanote.Source.Dynamic`. The previous
   `mapFsChanges` collected all per-file `(model -> model)` closures
   into a list via `mapM` and composed them with `foldl' ($)`. On the
   initial mount of a 4500-note notebook this kept *every* per-file
   closure (and the parsed Pandoc tree each one captured) alive until
   the fold ran. The new variant takes a handler of shape
   `Change -> model -> m model` so the loader can fold per-file
   updates through the model directly, dropping each closure as soon
   as its update has been applied.

| Attempt                                    | Small fixture (5 KiB/note) | Issue-scale fixture (15 KiB/note) |
| ------------------------------------------ | -------------------------: | --------------------------------: |
| master @ 5518fb25 (3-run median)           |               **1268 MiB** |                      **2420 MiB** |
| + `evaluate . force` after parse           |   1230 MiB (within noise)  |                                — |
| + `NoteContent` ADT                        |   1230 MiB                 |                          2563 MiB |
| + `_relCtx :: !Text`                       |   1248 MiB                 |                          2592 MiB |
| + pre-extract indices, `!relTo / !relFrom` |   1238 MiB                 |                          2563 MiB |
| + `unionMountStreaming` + `T.copy(url,attrs)` | 1300 MiB *(regression)* |                  2473 MiB *(parity)* |

**The architectural changes do not move the headline RSS at issue
scale.** The closure-type profile after every attempt shows
`Sequence.Internal.Node*` collapse from ~470 MB to single kilobytes —
the per-note Pandoc tree really *is* gone from `_modelNotes` — but
~140 MiB of `Pandoc.Str` cells and ~285 MiB of `Text` headers stay
resident, just under different parents. The same closure mass appears
in the master profile and in the post-fix profile. **Something is
re-pinning the parsed Pandoc tree in both cases**, and the four
architectural moves above are not what's holding it.

The PR therefore reverts the architectural changes and lands only the
investigation infrastructure. The fix itself needs to come *after*
isolating the actual retainer.

## Where the leak likely lives — candidates I haven't ruled out

The remaining ~140 MiB Pandoc.Str + ~285 MiB Text + ~85 MiB ARR_WORDS,
present in *both* master and the post-fix builds, must be pinned by
one of:

- **The heap-profile sample window itself.** `+RTS -hT -i<N>` samples
  on MUT time. After loading completes the live server is idle (no
  MUT time accumulating), so the last profile sample is captured at
  ~47s — right at the edge of the load phase, when in-flight per-file
  Pandoc trees may not yet have been drained by GC. A diagnostic that
  forces `performMajorGC` post-load and then dumps a profile would
  disambiguate.
- **`_modelHeistTemplate :: TemplateState`.** Heist's parsed templates
  are XML splices, not Pandoc; but if the template state's internal
  cache or the precomputed splice closures retain Pandoc fragments
  via the renderer dictionary, that'd scale linearly with template
  count. Worth a sample on a fixture with N templates × 1 note vs 1
  template × N notes to disentangle.
- **Aeson `_noteMeta`.** `applyNoteMetaFilters` runs `W.query` over
  the doc to extract description/image/inline-tags, storing the
  results as `Aeson.Value`. The results are `Text` values, but if
  Aeson's NFData isn't actually deep on the path we're using, a
  `Value` thunk could pin the doc. `evaluateNF_` calls in the load
  path should rule this in or out.
- **`_noteTitle :: Tit.Title`.** When a note has no `title:` in its
  frontmatter, the H1 inlines become `TitlePandoc [B.Inline]`. My
  fixture sets `title:` on every note so all titles are
  `TitlePlain Text` — but a real notebook of 4500 notes likely has a
  mix. A fixture variant with H1-derived titles would expose this.
- **`commonmark-simple` / `commonmark` parser internals.** The
  closure profile shows ~10 K `Commonmark.Tokens.Tok` retained.
  Tokens should be transient; if they're cached or referenced by the
  parser's internal state across calls, that's an upstream leak.
- **`Heist.Extra.Splices.Pandoc.TaskList.queryTasks`** retains
  `[(Bool, [B.Inline])]`. My fixture has zero `- [ ]` items so this
  shouldn't apply, but it's the obvious next per-note retainer once
  `_relCtx` is plain text.

## Next pass

The mechanical next step is an emanote-side diagnostic: add a
`performMajorGC` + heap-stats print once the model status flips to
`Status_Ready`, and add a one-shot `--dump-heap-after-load <file>`
flag that triggers a `-hT` profile capture at a known point. With
that, the next PR can attribute the residual 140 MiB of `Pandoc.Str`
to a single retainer (or to "transient parser state in flight, not
actually leaked"), and the architectural moves above either become
the right fix or a different fix targets the now-isolated site
directly.

The investigation infrastructure landing in this PR — the fixture
generator, the harness, the doc — is what that next pass will start
from.
