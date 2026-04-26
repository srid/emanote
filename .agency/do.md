# /do config

## Check command
cabal build all

## Format command
just fmt

## Test command
- `just test` — Haskell unit tests
- `just e2e-live` / `just e2e-static` — cucumber+Playwright e2e (live and static modes)

## CI command
vira ci

## Documentation
Keep `README.md`, `docs/` (user documentation), and `CHANGELOG.md` (under the `Unreleased` section) in sync with user-facing changes.

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
