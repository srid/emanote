---
slug: ralph-memory-66-deferred
---

# Defer the per-Note Pandoc AST (#66)

A second pass at [issue #66](https://github.com/srid/emanote/issues/66),
implemented from scratch as a fresh take on
[PR #678](https://github.com/srid/emanote/pull/678)'s approach. PR
[#740](https://github.com/srid/emanote/pull/740) shipped the
behaviour-preserving local fixes (`deepseq` + `-F1.5` + `_relCtx`
removal, **−29%**); this branch attacks the structural ceiling those
cycles deliberately left in place.

## What changes structurally

`Note._noteDoc :: Pandoc` is no longer a stored value. It is now a
**lazy field bound to a re-parse closure** over `_noteSourceText`.
First access on the render path forces the closure (pure markdown
re-parse, no Lua); Haskell's normal memoisation then writes the result
back into the field so subsequent accesses within the same `Note`
reference are free.

Three categories of notes still bind `_noteDoc` to the
already-evaluated Pandoc because a pure re-parse cannot reproduce
their post-filter shape:

| Retention reason     | Source                                              |
| -------------------- | --------------------------------------------------- |
| Lua filter declared  | `pfdParseFilters` / `pfdRenderHtmlFilters` non-empty |
| Synthetic note       | `mkEmptyNoteWith` (folder placeholders, banners)    |
| Parse errors         | `_noteErrors` non-empty (banner Block prepended)    |

Two new pre-extracted summary fields land at parse time so neither
`noteRels` (link index) nor `noteTasks` (task index) needs to walk the
full Pandoc:

```haskell
data Note = Note
  { _noteSourceText  :: !Text
  , _noteLinks       :: ![NoteLink]
  , _noteTaskList    :: ![NoteTask]
  , _noteDoc         :: Pandoc      -- lazy on purpose
  , …
  }
```

`parseAndInsert` deepseq-forces only `_noteLinks` and `_noteTaskList`
at insert time — never `_noteDoc`. Forcing `_noteDoc` would defeat the
optimisation by materialising the re-parse closure into a Pandoc
immediately. The two summary fields would otherwise hold thunks
closing over the post-filter Pandoc that `parseNote` produced, keeping
it alive after the local `note` reference is dropped.

`mkNoteWith` also branches **eagerly** on whether the note must retain
its Pandoc — a single `if mustRetain then doc else deferredDoc`
expression as the field value would keep both branches alive in a
thunk closure (`doc` *and* `deferredDoc`). Two separate
record-construction branches commit each Note's `_noteDoc` to exactly
one of them.

`Eq` / `Ord` for `Note` are now manual and compare by `_noteRoute`
alone. The derived versions would force the lazy `_noteDoc` on every
IxSet insertion-time comparison, re-parsing the AST for every other
note already in the set — quadratic, and exactly the work we're
trying to avoid.

`modelLookupBacklinks` re-extracts per-link context from the source
note's Pandoc on demand via a new `noteRelCtxToTarget`, replacing the
contexts that PR #740's cycle 3 already stopped storing inside `Rel`.

## Final result (5-run median on 4.5k synthetic corpus, `+RTS -N`)

| Metric        | Baseline @6950760d | This PR  | Δ |
| ------------- | -----------------: | -------: | --- |
| `READY` (s)   |                 51 |       54 | +6 % |
| `LOAD_HWM`    |           5165 MiB | 1660 MiB | **−67.9 %** |
| `AFTER_HWM`   |           5181 MiB | 2219 MiB | **−57.2 %** |

For the smaller 1k corpus the absolute numbers drop to ~543 MiB load
peak / ~610 MiB after-hits (~−53 % vs the same 1300 MiB baseline).

## Methodology

Identical to PR #740 — synthetic 4501-file / 72.4 MiB corpus produced
by `docs/dev/ralph/memory-66-deferred/gen_corpus.py` (fixed seed),
measured on `srid1` (32 cores, 125 GiB RAM, NixOS, no swap) via
`measure.sh`. The driver starts `emanote run`, polls `curl /` until
ready, samples `/proc/<pid>/status` for `VmRSS` / `VmHWM`, then pings
six pages and re-samples.

Tests (`cabal test emanote`): 125 examples, **0 failures**. The
`RelSpec` source-order test was rephrased to assert on `_relSrcPos`
directly (since `_relCtx` no longer carries context — same change as
PR #740 cycle 3).

## Known limitation: dense-embed render latency on this synthetic corpus

The synthetic corpus is intentionally pathological — every note
contains an average of one `![[embed]]` and a handful of `[[wiki]]`
links to other random notes. With deferred parsing, rendering an
embed-heavy page re-parses every transitively embedded note on first
access; pages like `/topic00/n00032` (19 direct embeds, transitive
fan-out into hundreds of notes) exceed the 30s curl timeout on a cold
process.

This is a real limitation of the "memoise into the lazy field"
approach: the first render of a hub note is bounded by the size of the
embed graph reachable from it. Subsequent renders of the same hub are
fast because each unique embedded note has been forced once.

Mitigations not implemented in this PR:

* a stripped-down "summary Pandoc" for embed bodies (PR #678's
  approach), trading retention for a fixed-cost embed render;
* request-scoped Pandoc cache plumbed through `RenderCtx`;
* an `unsafePerformIO` `IORef` per Note for explicit cache eviction.

On the real `docs/` notebook the slowness does not surface in practice
because the embed graph is shallow (RSS settles at ~255 MiB).

## Files changed

* `emanote/src/Emanote/Model/Note.hs` — new fields, lazy `_noteDoc`,
  manual `Eq`/`Ord`/`Show` skipping the AST, `reparseMd` / `reparseOrg`
  pure re-parse helpers.
* `emanote/src/Emanote/Model/Link/Rel.hs` — `noteRels` reads
  `_noteLinks` (no Pandoc walk), `noteRelCtxToTarget` recomputes on
  demand.
* `emanote/src/Emanote/Model/Task.hs` — `noteTasks` reads
  `_noteTaskList` (no Pandoc walk).
* `emanote/src/Emanote/Model/Graph.hs` — `modelLookupBacklinks` uses
  `noteRelCtxToTarget`.
* `emanote/src/Emanote/Pandoc/Renderer/Embed.hs` — binds
  `noteDoc` once per embed so body + TOC splices share the forced
  thunk.
* `emanote/src/Emanote/Source/Patch.hs` — deepseq summary fields at
  insert time (but not `_noteDoc`).
* `emanote/test/Emanote/Model/Link/RelSpec.hs` — assert source order
  via `_relSrcPos`.

## Relationship to PR #740 and PR #678

PR #740 (cycles 1-3) and this PR are **additive** — applying both is
the recommended path:

| Layer         | Change                                          | Δ AFTER_HWM |
| ------------- | ----------------------------------------------- | ----------- |
| baseline      | origin/master @6950760d                         | 5181 MiB    |
| PR #740 ⊕     | `deepseq` + `-F1.5` + `_relCtx` drop            | 3672 MiB    |
| this PR alone | deferred `_noteDoc`                             | 2219 MiB    |
| (both)        | not yet measured                                | TBD         |

This PR's design is parallel to PR #678's
`NoteBodyParsed`/`NoteBodyDeferred` sum type but lands the same
optimisation through a single record + a lazy field, avoiding the
extra type and the embedded "summary Pandoc" — at the cost of the
dense-embed render-latency limitation above.
