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
- The temporary notebooks were generated under `/tmp` and removed/recreated for each run.
- Python, curl, and shell tooling came from `nix develop`.

## Workload

The current local workload is smaller than the issue reporter's notebook but large enough to reproduce more than 1 GiB resident memory:

- Markdown file count: 2401 (`index.md` plus 2400 notes).
- Notebook size on disk: 19M.
- Startup content: every note has YAML front matter, repeated Markdown, code fences, tags, and six wikilinks.
- Update phase: 3 rounds, 240 rewritten notes per round, 720 rewritten notes total.
- Server command shape:

```sh
emanote --allow-broken-internal-links -L "$NB" run --host 127.0.0.1 --port "$PORT"
```

## Runs so far

| Run | Binary | RTS | Startup readiness | First idle RSS | Peak RSS | Peak phase | Final RSS | Notes |
| --- | --- | --- | --- | ---: | ---: | --- | ---: | --- |
| Baseline | `/nix/store/rym5k9jidv5rrgg59b0cxkljr5r314wh-emanote-1.6.0.0/bin/emanote` | default `-N` | first idle sample at 42s; exact curl-success timestamp was not captured | 1244.6 MiB | 1293.3 MiB | final-idle | 1293.3 MiB | `/tmp/emanote-issue66-baseline-1g-v2/rss.tsv` |
| Strict-note-force patch | `/nix/store/pmy698y05hkn1bqkv0s6m744ap6i04hf-emanote-1.6.0.0/bin/emanote` | default `-N` | 26s | 1231.0 MiB | 1283.5 MiB | update-round-3 | 1283.5 MiB | `/tmp/emanote-issue66-fixed-1g-v2/rss.tsv` |
| Strict-note-force patch | `/nix/store/pmy698y05hkn1bqkv0s6m744ap6i04hf-emanote-1.6.0.0/bin/emanote` | `+RTS -N1 -RTS` | 24s | 1216.3 MiB | 1216.3 MiB | update-round-3 | 1216.3 MiB | `/tmp/emanote-issue66-fixed-n1/rss.tsv` |
| Per-note compacted `Pandoc` experiment | `/nix/store/viw7fvfd0255wx8ap07qd2dc4hj343ig-emanote-1.6.0.0/bin/emanote` | default `-N` | not used as candidate | not used as candidate | 12601.0 MiB before manual kill | update-round-2 | not recorded | `/tmp/emanote-issue66-compact-v1/rss.tsv`; rejected because RSS became much worse |

## Heap profile

The profiling executable was built with:

```sh
nix develop -c cabal build exe:emanote --enable-profiling --enable-library-profiling --ghc-options='-fprof-auto -rtsopts'
```

The profiled workload used the same 2401-file notebook shape as the RSS runs above. The readiness probe requested `/`, so render/index work is present in the cost-centre profile.

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

The profile does not support treating laziness alone as the root cause: forcing parsed notes reduced RSS by only 9.8 MiB on the default-RTS run. The sampled heap is dominated by retained `List`, `Text`, `ARR_WORDS`, and `Inline` payloads produced by parsing, link traversal, and rendering paths. The next code experiment should therefore reduce retained Pandoc/link-context payload and then re-measure with the same workload.

## Current factual conclusions

- The local reproduction crossed 1 GiB RSS: the baseline run peaked at 1293.3 MiB.
- The local reproduction includes updates, not only startup: 720 notes were rewritten after the server became ready.
- The first strictness patch does compile and slightly changes the measured RSS on this workload, but it does not make Emanote slim. Baseline peak was 1293.3 MiB; strict-note-force peak with default RTS was 1283.5 MiB.
- Running with one RTS capability reduced peak RSS to 1216.3 MiB on the strict-note-force binary, which is still above 1 GiB for this workload.
- Per-note `GHC.Compact` storage is not viable for this workload: that experiment reached 12601.0 MiB RSS during update-round-2 before it was killed.
- Exact baseline startup readiness time still needs a rerun that records the curl-success timestamp.
- Before/after RSS and startup time must be rerun after any code change justified by the heap profile.
