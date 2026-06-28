---
name: ci
description: Reference for the `odu` runner — how to invoke a full pipeline, a single recipe, or a platform-pinned node, and how to attach to a live run, from a project whose CI odu runs. Trigger when the user asks to "run CI", "run the pipeline", "re-run a check", to run named lanes or recipes (e.g. "run fmt and nix", "just the e2e lane", bare selectors like `fmt`/`nix`/`e2e`), or names a recipe by `<recipe>@<platform>`. This skill — not a repo's local `just ci` / `just <recipe>` — is how an odu-run request is served.
---

# odu

[`odu`](https://github.com/juspay/odu) (Tamil ஓடு — "run") runs the `just`
recipe DAG tagged `[metadata("ci")]` across platforms and posts GitHub
commit statuses per `<recipe>@<platform>` context. Unlike batch runners,
the run is **live state you attach to**: the coordinator serves a typed
surface on `.ci/odu.sock`, so `status`/`logs`/`attach` are in-band — no
process-compose, no separately-versioned socket client.

> **A request to run CI is a request to run `odu` — never `just ci`.** Many
> consuming repos expose a `just ci` (or `just <recipe>`) target that runs a
> pipeline locally. Do **not** shell out to it: it is a parallel, non-attachable
> path that bypasses everything odu gives you — the live surface, per-node GitHub
> statuses, structured results, fail-fast, `cancel`/`supersede`, and the log
> resources below. "run CI", "run fmt and nix", "re-run the e2e lane" all mean
> *drive an odu run*, by the MCP face first and the `odu` CLI otherwise.
>
> **Prefer the MCP face for runs.** When the `odu-mcp` skill is present (the
> `mcp__odu__*` tools — check for an odu MCP server before shelling out), drive
> runs through it — `run` (pass `selectors` for named lanes/recipes) →
> `wait_for_settle` (fail-fast) → read the red node's log → `node_rerun`, with
> `cancel` / `run({supersede})` to call off or replace a run. It spawns the same
> coordinator but gives you structured results and the fail-fast loop instead of
> scraping terminal output. The `nix run … -- run` CLI below is the reference and
> the fallback when no MCP server is wired.
>
> **Logs are a resource, not a tool.** Don't look for a log-tail tool — there
> isn't one. A node's output is the MCP **resource** `surface://collections/logs/{id}`
> (`{id}` is the node, e.g. `ci::unit@aarch64-darwin`), read with
> `ReadMcpResourceTool`: the live buffered tail while the run is up, else the
> durable per-SHA log on disk. So when `wait_for_settle` returns a red node, the
> "read the log" step is `ReadMcpResourceTool` on that node's
> `surface://collections/logs/{id}` — subscribe for push updates, or just re-read
> to poll. (`surface://streams/nodes` is the pipeline snapshot resource alongside
> it.)

## Invoking

```sh
nix run github:juspay/odu -- <subcommand> [args]
```

Pin a ref for reproducibility, or — if the consuming repo npins-pins odu
and re-exports it (kolu does) — prefer its own flake output so the version
is repo-controlled:

```sh
nix run .#odu -- <subcommand> [args]
```

## Modes

**Strict by default** — `odu run` refuses a dirty tree, pins `HEAD` via
`git worktree`, posts commit statuses, and splits per-recipe logs into
`.ci/<sha>/<plat>/<recipe>.log`. Three flags relax that policy:

| Flags | Tree | HEAD pin | Status posts | Use for |
| --- | --- | --- | --- | --- |
| _(none — default)_ | clean (refuses dirty) | `git worktree` at HEAD | posted | "real" CI runs |
| `--no-post` | clean | `git worktree` at HEAD | _none_ | non-GitHub strict consumers; debugging strict without writing the PR's check list |
| `--no-snapshot` (implies `--no-post`) | live working tree | none | _none_ | strict-mode dev iteration without clean-tree refuse |
| `--no-strict` (meta — same as `--no-snapshot --no-post`) | live working tree | none | _none_ | dev iteration; the one-flag opt-out for "just run the pipeline" |

Every mode ends with the same `── ci run summary @ <sha7> ──` verdict block
(the sha reads `<sha7>+dirty` for a live-tree run on uncommitted changes)
and exits non-zero if any node failed or errored.

## Common invocations

```sh
# Full pipeline (the [metadata("ci")] root, every configured platform).
nix run github:juspay/odu -- run

# Dev iteration on a dirty tree: no clean-tree refuse, no HEAD pin, no posts.
nix run github:juspay/odu -- run --no-strict

# Re-run a single failed recipe on one lane — overwrites the same GitHub
# commit-status context the full run wrote (closes the red check).
nix run github:juspay/odu -- run e2e@x86_64-linux

# One recipe across every pipeline platform; selectors compose.
nix run github:juspay/odu -- run e2e lint

# Restrict the WHOLE fanout to one platform (repeatable).
nix run github:juspay/odu -- run --platform x86_64-linux

# Skip the dependency closure; run ONLY the named nodes (_ci-setup still rides).
nix run github:juspay/odu -- run --no-deps e2e@aarch64-darwin

# A different DAG root instead of the [metadata("ci")] recipe.
nix run github:juspay/odu -- run --root ci::e2e

# One-shot redirect of a platform's host (how a pool-lease wrapper pins a box).
nix run github:juspay/odu -- run --host x86_64-linux=my-build-box

# One NDJSON line per node transition, for agents/tools driving CI:
# {"node":"ci::e2e@x86_64-linux","recipe":"ci::e2e","platform":"x86_64-linux",
#  "status":"running|success|failed|skipped|errored","exit_code":1,
#  "log":".ci/<sha7>/x86_64-linux/ci::e2e.log"}
nix run github:juspay/odu -- run --progress json
```

Without `--progress json`, output adapts to where stdout points: a live
colour lane-matrix with a log-tail footer on a TTY; quiet transition lines
plus a once-a-minute "… still running" heartbeat when piped.

## Inspection subcommands (no side effects)

```sh
nix run github:juspay/odu -- dump            # resolved pipeline as JSON
nix run github:juspay/odu -- graph           # dependency graph (Mermaid)
nix run github:juspay/odu -- protect --dry-run   # the (recipe × platform) contexts
nix run github:juspay/odu -- protect             # PATCH branch protection to them
```

## Live introspection (attach to a run in progress)

While `odu run` is live in a checkout, these attach to its surface over
`.ci/odu.sock`:

```sh
nix run github:juspay/odu -- status          # snapshot; -o json for tooling
nix run github:juspay/odu -- attach          # live TUI dashboard on a tty
                                             # (digits attach · n/p cycle ·
                                             #  r rerun · q quit); -o json
                                             # = transition stream
nix run github:juspay/odu -- logs -f e2e@x86_64-linux
nix run github:juspay/odu -- cancel          # stop the live run, cleanly
```

No run in progress ⇒ exit non-zero with `no run in progress in this
checkout (no live socket at .ci/odu.sock)`. One run per checkout — a
second `odu run` refuses while the socket is live.

**Cancel / supersede / linger.** `odu cancel` drives the live run's teardown
from a second process (finalize posted statuses, close lanes, drop the socket)
and waits until it's gone — no need to wait out a doomed run or `pkill` the
coordinator. `odu run --supersede` cancels whatever's live here first, then
starts ("stop this, run the fixed commit"). By default a run exits the instant
it drains; `odu run --linger` keeps it serving past settle so a node can be
rerun later (retry a flake), self-reaping after an idle period or on `cancel`.

## Hosts config

`$ODU_HOSTS` (a file path) → `~/.config/odu/hosts.json` → fallback
`~/.config/justci/hosts.json` (zero-config migration from justci):

```json
{
  "x86_64-linux": "my-linux-builder",
  "aarch64-darwin": "me@mac-mini.local"
}
```

Keys are Nix system tuples; values are anything ssh dials, or `localhost`
(runs directly against the snapshot, no closure copy). Missing platforms
silently drop from the fanout. `--host PLAT=ADDR` overrides per run.

A lane host needs only **ssh + Nix + outbound https**: the runner ships as
a Nix closure (`nix copy` → realise on the host), and the source arrives by
`git fetch` of the **pushed** SHA — remote lanes cannot test unpushed
commits (no git-bundle transport; push first). The lane host's own nix is
used on the runner's PATH (never a pinned client — version skew against the
host daemon corrupts CA-derivation handling).

## Semantics worth knowing

- **Lanes are one-shot**: a lane whose ssh link dies mid-run fails as
  `errored` (GitHub state `error`, `Errored (<dur>)` description); live
  state does not survive a runner restart — the per-SHA log files do.
- **Skipped nodes post no status**: an absent required context is what
  blocks the merge.
- The coordinator resolves the **generic lane runner from odu's own flake**,
  not the repo under test:
  `nix eval $ODU_RUNNER_FLAKE#packages.<platform>.odu-runner.drvPath`, where
  `ODU_RUNNER_FLAKE` is baked onto the `odu` wrapper from `self.outPath` at
  build time. A consuming repo no longer re-exports `odu-runner`. There is no
  override or fallback — the runner is the exact build that shipped the
  coordinator (they share an RPC contract); a binary built without the baked
  flake refuses to run.

## When NOT to use this skill

- Questions about odu's internals or design history — read the
  [README](https://github.com/juspay/odu/blob/master/README.md) and the
  kolu Atlas note
  [*A CI runner you attach to*](https://github.com/juspay/kolu/blob/master/docs/atlas/dist/mini-ci-vs-justci.html).
- Project-specific CI operations (warm pools, host leases, banned flags)
  — that's the consuming repo's operational docs, layered on top of this
  reference.
