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

## Current factual conclusions

- The local reproduction crossed 1 GiB RSS: the baseline run peaked at 1293.3 MiB.
- The local reproduction includes updates, not only startup: 720 notes were rewritten after the server became ready.
- The first strictness patch does compile and slightly changes the measured RSS on this workload, but it does not make Emanote slim. Baseline peak was 1293.3 MiB; strict-note-force peak with default RTS was 1283.5 MiB.
- Running with one RTS capability reduced peak RSS to 1216.3 MiB on the strict-note-force binary, which is still above 1 GiB for this workload.
- The next decision needs heap profile data. The strictness patch alone is not enough evidence for a real fix.

## Profiling plan

The profiling executable was built with:

```sh
nix develop -c cabal build exe:emanote --enable-profiling --enable-library-profiling --ghc-options='-fprof-auto -rtsopts'
```

Next measurements to add:

- Heap profile by cost centre on the 2401-file workload.
- Heap profile by closure type on the same workload.
- Exact baseline startup readiness time from a rerun that records the curl-success timestamp.
- Before/after RSS and startup time after any code change justified by the heap profile.

