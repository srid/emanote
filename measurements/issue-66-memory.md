# Issue 66 memory measurements

This file tracks measurements for [issue #66](https://github.com/srid/emanote/issues/66).

## Reported issue facts

The GitHub issue reports:

- Notebook size: 69M.
- Markdown file count: 4561.
- Startup load time: about 1 minute.
- Resident memory: about 4.7 GB.

## Local environment

- Machine: AMD Ryzen 7 8845HS, 16 logical CPUs.
- OS source for RSS: `/proc/$PID/status`, field `VmRSS`.
- Measurement cadence: 1 sample per second.
- Python, curl-equivalent HTTP requests, and shell tooling came from `nix develop`.
- Nix store paths in the result tables identify the exact executables measured; later report-only edits can produce a different store path without changing the measured Haskell code.
- Server command shape:

```sh
emanote --allow-broken-internal-links -L "$NB" run --host 127.0.0.1 --port "$PORT"
```

## Workloads

Two generated notebooks were used:

| Workload | Markdown files | Size on disk | Update phase |
| --- | ---: | ---: | --- |
| Local reproduction | 2401 (`index.md` plus 2400 notes) | 18M / 17.74 MiB | 3 rounds, 240 rewritten notes per round, 720 rewrites total |
| Issue-sized check | 4561 (`index.md` plus 4560 notes) | 82M / 81.65 MiB | 3 rounds, 456 rewritten notes per round, 1368 rewrites total |

Each note has YAML front matter, repeated Markdown text, inline tags, blockquotes, and six wikilinks. `index.md` links to every note so root rendering exercises the full route tree and title lookup path.

## RSS and startup results

![Peak RSS comparison](https://quickchart.io/chart?c=%7Btype%3A%27bar%27%2Cdata%3A%7Blabels%3A%5B%27Baseline%202401%20timeout%27%2C%27Fixed%202401%27%2C%27Fixed%204561%2F82M%27%5D%2Cdatasets%3A%5B%7Blabel%3A%27Peak%20RSS%20MiB%27%2Cdata%3A%5B1478.5%2C457.3%2C752.2%5D%7D%5D%7D%7D)

| Run | Binary | Workload | Startup readiness | First idle RSS | First `/` render | Peak RSS | Peak phase | Final RSS | Raw data |
| --- | --- | --- | ---: | ---: | ---: | ---: | --- | ---: | --- |
| Baseline reproduction | `/nix/store/rym5k9jidv5rrgg59b0cxkljr5r314wh-emanote-1.6.0.0/bin/emanote` | 2401 files, 19M | no response before 300s timeout | not reached | not reached | 1478.5 MiB | startup | not reached | `/tmp/emanote-issue66-baseline-current-harness-v1/rss.tsv` |
| Fixed | `/nix/store/g3xnnpa6llqz509rs5g2wil4vaqj0zzj-emanote-1.6.0.0/bin/emanote` | 2401 files, 18M | 3.583s | 387.3 MiB | 2.571s / 4240221 bytes | 457.3 MiB | final-idle | 457.3 MiB | `/tmp/emanote-issue66-final-2401-v8/rss.tsv` |
| Fixed, issue-sized | `/nix/store/g3xnnpa6llqz509rs5g2wil4vaqj0zzj-emanote-1.6.0.0/bin/emanote` | 4561 files, 82M | 9.690s | 703.4 MiB | 7.688s / 8007261 bytes | 752.2 MiB | final-idle | 752.1 MiB | `/tmp/emanote-issue66-final-4561-69m-v8/rss.tsv` |

Earlier baseline runs also reproduced the >1 GiB condition:

| Run | Binary | Workload | Startup readiness | First idle RSS | Peak RSS | Raw data |
| --- | --- | --- | ---: | ---: | ---: | --- |
| Baseline | `/nix/store/rym5k9jidv5rrgg59b0cxkljr5r314wh-emanote-1.6.0.0/bin/emanote` | 2401 files, 19M | first idle sample at 42s; exact curl-success timestamp was not captured | 1244.6 MiB | 1293.3 MiB | `/tmp/emanote-issue66-baseline-1g-v2/rss.tsv` |
| Strict-note-force only | `/nix/store/pmy698y05hkn1bqkv0s6m744ap6i04hf-emanote-1.6.0.0/bin/emanote` | 2401 files, 19M | 26s | 1231.0 MiB | 1283.5 MiB | `/tmp/emanote-issue66-fixed-1g-v2/rss.tsv` |

## Output integrity checks

The issue-sized fixed run was checked after measurement with the same generated 4561-file notebook:

| Check | Result |
| --- | ---: |
| Readiness | 9.690s |
| Unique `note-NNNN` tokens in rendered `/` | 4560 |
| Rendered `/` contains `note-0001` | True |
| Rendered `/` contains `note-4560` | True |
| Rendered `/` bytes | 8007261 |
| Rendered `/note-0001` has heading | True |
| Rendered `/note-0001` has body text | True |
| Rendered `/note-0001` bytes | 5077695 |

## Heap profile

The profiling executable was built with:

```sh
nix develop -c cabal build exe:emanote --enable-profiling --enable-library-profiling --ghc-options='-fprof-auto -rtsopts'
```

The profiled workload used the same 2401-file notebook shape as the RSS runs. The readiness probe requested `/`, so render/index work is present in the cost-centre profile.

| Profile | Path | Startup readiness | Profiler RSS peak | Sampled heap peak | Peak sample time |
| --- | --- | ---: | ---: | ---: | ---: |
| Cost centre (`-hc -i1 -p`) | `/tmp/emanote-issue66-prof-current-hc/emanote.hp` | not separately recorded | 1868.6 MiB | 383.0 MiB | 61.51s |
| Closure type (`-hy -i1 -p`) | `/tmp/emanote-issue66-prof-current-hy/emanote.hp` | 67s | 1893.5 MiB | 382.2 MiB | 61.51s |

Top cost-centre entries at the 383.0 MiB sampled-heap peak:

| Sampled heap | Cost centre label |
| ---: | --- |
| 80.6 MiB | `(7041)walkBlockM/walkPandoc...` |
| 65.9 MiB | `(6159)text/defaultInlinePar...` |
| 20.9 MiB | `(8629)hashTag/hashTagSpec...` |
| 16.6 MiB | `(12221)rawApply/applyNodes...` |
| 15.2 MiB | `(4876)normalize/tokenize...` |
| 14.8 MiB | `(7050)walkInlineM/walkBlock...` |
| 12.7 MiB | `(6707)wikilinkInline/wikilink...` |
| 9.2 MiB | `(7900)unChunks/mkInlinePars...` |
| 8.6 MiB | `(9007)hashTag.classes/hashTag...` |
| 8.6 MiB | `(9015)linkifyInlineTags...` |

Top closure-type entries at the 382.2 MiB sampled-heap peak:

| Sampled heap | Closure type |
| ---: | --- |
| 139.7 MiB | `List` |
| 79.7 MiB | `Text` |
| 53.2 MiB | `ARR_WORDS` |
| 34.9 MiB | `Inline` |
| 13.1 MiB | `Tuple2` |
| 11.6 MiB | `HeistState` |
| 8.3 MiB | `->List` |
| 8.1 MiB | `Tuple3` |
| 4.7 MiB | `SMALL_MUT_ARR_PTRS_FROZEN_CLEAN` |
| 3.8 MiB | `Map` |

## Code changes made

The fix avoids retaining full Pandoc documents for simple Markdown notes at model-build time. A simple Markdown note now stores route, title, metadata, tags, source text, and one representative wikilink relation per unresolved target. The full Pandoc parse is deferred until the note is rendered, and backlink pages recover repeated deferred-note contexts from the stored source text on demand.

Other code changes:

- Batch model updates for groups of changed LML files so `modelNotes`, `modelRels`, and `modelTasks` are replaced together.
- Replace the ad hoc lazy-note fields with explicit `NoteBody`, `DeferredNote`, and `ParseContext` types.
- Build folgezettel child adjacency once per tree construction instead of scanning relations for every node.
- Compute the active folgezettel ancestor set once per route-tree render.
- Enable only the Markdown syntax extensions whose marker text appears in the document.
- Fully parse lazy notes before rendering HTML, Atom feeds, and embedded-note templates, including nested embeds.

## Experiments rejected

These were measured and not used as the final fix:

| Experiment | Result |
| --- | --- |
| Force parsed notes with `NFData` only | Peak RSS changed from 1293.3 MiB to 1283.5 MiB on the earlier 2401-file workload. This did not materially fix the issue. |
| Per-note `GHC.Compact` storage | Reached 12601.0 MiB RSS during update-round-2 before manual kill: `/tmp/emanote-issue66-compact-v1/rss.tsv`. |
| Drop retained backlink context only | Peak/final 1332.8 MiB: `/tmp/emanote-issue66-no-relctx-v2/rss.tsv`. |
| Drop stored note body only | Peak/final 1424.6 MiB: `/tmp/emanote-issue66-drop-doc-v1/rss.tsv`. |
| Drop stored note body and backlink context | Peak/final 1333.2 MiB: `/tmp/emanote-issue66-drop-doc-no-relctx-v1/rss.tsv`. |
| Structural `NoteBody` refactor without deferred relation deduplication | On the 4561-file, 81.65 MiB workload, peak/final RSS was 1688.7 MiB: `/tmp/emanote-issue66-final-4561-69m-v4/rss.tsv`. |
| Sharing the line context for repeated deferred-note wikilinks only | On the same 4561-file, 81.65 MiB workload, peak/final RSS was 1711.7 MiB: `/tmp/emanote-issue66-final-4561-69m-v5/rss.tsv`. This was noise/regression, not a fix. |
| RTS `-N1 -c` controls | Reduced RSS in some runs, but this PR does not rely on RTS defaults as the fix. |

## Current factual conclusions

- The memory issue was reproduced locally above 1 GiB RSS: the baseline reached 1478.5 MiB while still in startup on a 2401-file generated notebook, and an earlier baseline run peaked at 1293.3 MiB.
- Heap profiling showed the retained heap was dominated by `List`, `Text`, `ARR_WORDS`, and `Inline` values from Pandoc parsing/link traversal/rendering, not by a laziness-only leak.
- The code-level lazy model parse plus deferred relation deduplication reduced the 2401-file workload from a baseline that failed to answer before 300s to 3.583s readiness and 457.3 MiB peak RSS.
- On the 4561-file, 82M generated notebook, the fixed binary reached readiness in 9.690s and peaked at 752.2 MiB after 1368 note rewrites.
- On the same 4561-file, 82M generated notebook, keeping every repeated deferred-note relation peaked at 1688.7 MiB. Keeping one representative relation per source note and unresolved target reduced that to 752.2 MiB.
