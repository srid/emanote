# /do config

## Check command
`cabal build all`

## Format command
`just fmt`

## Test command
- `nix develop -c cabal test all` — Haskell unit tests
- `just e2e-live` / `just e2e-static` / `just e2e-morph` — cucumber+Playwright e2e (live, static, and morph modes)

## CI command
- `vira ci`
- All e2e tests (`just e2e-??`)

Ignore Github Actions (slow) unless user asks for it.

## Documentation
Keep `README.md`, `docs/` (user documentation), and `CHANGELOG.md` (under the `Unreleased` section) in sync with user-facing changes.

**CHANGELOG style: one concise line per PR.** Each user-facing change gets a single bullet — issue/PR number, the headline, and a one-line link to where the detail lives (a docs page, a deep-dive `[[wikilink]]`, the issue, …). The detail belongs on the docs page or in the PR description, not in the CHANGELOG. Multi-paragraph entries that re-explain the design are clutter; if a reviewer needs more than the link, the docs page wasn't pulling its weight.

New or fixed **Markdown-syntax features** should be demonstrated in [`docs/guide/markdown.md`](../docs/guide/markdown.md) so the live example serves as both reference and regression check. A working `<details>` block, a new callout type, a new wiki-link form — each goes there as a real rendered sample, not just a CHANGELOG note.

## HACK comments

Mark workarounds, brittle protocols, and structural compromises with a leading `HACK:` so they're greppable and so reviewers know which code is load-bearing versus which is mortar. A `HACK:` comment must answer three questions:

1. **What contract does it depend on, and where does the other side live?** (Class string in two files? Env var the wrapper sets? nixpkgs path layout?)
2. **What goes wrong if either side drifts?** (Silent miss? Loud crash? Test failure?)
3. **What would the proper fix look like?** (Upstream PR + version bump? Typed protocol? Closure walker?)

A workaround whose proper fix is "patch upstream + wait for release" is a HACK. Vendoring with patches is *not* a HACK (it's the upstream protocol for forks). Defensive `try/catch` for a known framework guarantee is *not* a HACK (it's just clutter — delete it per CLAUDE.md). When in doubt: if removing the workaround would break the feature *and* you can name an outside change that would obsolete the workaround, mark it.

Mention each `HACK:` block in the PR description under a `## Hacks` heading with a one-line summary and a link to the file+line, so reviewers can audit them as a set instead of grep-spelunking.

## PR evidence

When the change has visible UI impact (theme, layout, rendering, navigation), post a `## Evidence` PR comment with screenshots. Use judgment — backend-only diffs (parser, link resolver, model) sometimes ripple into rendering and warrant a shot anyway.

**Delegate to a subagent** (`Agent(subagent_type="general-purpose", model="sonnet")`) so the main context stays clear of MCP and screenshot noise. Brief it with: the dev-server URL, what scenarios to capture, a `/tmp/emanote-evidence-<slug>.png` filename, and the PR number. Have it return only the markdown body it posted.

### Dev server

Spawn a dedicated emanote server on a **free random port** (the user may already have `just run` on 9010). Build the binary via Nix once, then point it at the user-docs notebook:

```sh
PORT=$(python3 -c 'import socket; s=socket.socket(); s.bind(("",0)); p=s.getsockname()[1]; s.close(); print(p)')
BIN="$(nix build --no-link --print-out-paths .#default)/bin/emanote"
"$BIN" -L docs run --port="$PORT" &
DEV_PID=$!
trap 'kill $DEV_PID 2>/dev/null' EXIT
```

For bug fixes that need a "before" shot, build master directly from GitHub — no clone or worktree needed — and run it on a different free port:

```sh
BIN_MASTER="$(nix --refresh build --no-link --print-out-paths github:srid/emanote/master)/bin/emanote"
```

`--refresh` ensures the flake input is re-fetched so you actually get tip-of-master.

### Capture, host, post

The subagent drives `chrome-devtools` MCP — `new_page` at `http://localhost:$PORT/`, navigates to the relevant note, `take_screenshot` to `/tmp/emanote-evidence-<slug>.png`.

`gh pr comment` can't attach binaries, so upload to a long-lived `evidence-assets` GitHub release and embed the download URL inline:

```sh
gh release view evidence-assets >/dev/null 2>&1 || \
  gh release create evidence-assets --prerelease \
    --title "Evidence assets (auto-uploaded by /do)" --notes "Do not delete."
gh release upload evidence-assets /tmp/emanote-evidence-<slug>.png --clobber
```

URL pattern: `https://github.com/srid/emanote/releases/download/evidence-assets/<filename>`. Use the single-quoted heredoc pattern (`<<'EOF'`) when posting so backticks and `$` survive unescaped.
