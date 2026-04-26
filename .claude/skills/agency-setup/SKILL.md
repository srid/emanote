---
name: agency-setup
description: Bootstrap or update srid/agency in this project — run apm via uvx, configure apm.yml, install skills, draft workflow instructions. Use for first-time setup or to refresh an existing install.
---

# Agency Setup

Configure (or refresh) this repo to use [srid/agency](https://github.com/srid/agency). Each step below is **idempotent** — it inspects what's already on disk and acts only on what's missing or out of date. The skill works equally well as first-time bootstrap, full refresh, or **partial-install upgrade** (e.g. user already added `srid/agency` to `apm.yml` manually but never created `workflow.instructions.md` — the skill detects the gap and fills it without re-doing the parts that already exist).

When the repo already has `srid/agency` in `apm.yml`, this skill also refreshes it to the latest ref (via `apm deps update srid/agency` in step 6) — there's no separate "update" mode.

Don't commit anything — leave changes staged for the user to review.

## Invariant: `apm install` and `apm compile` run *after* every file change

`apm install` regenerates the host folders (`.claude/`, `.opencode/`, `.codex/`) from `apm.yml` plus the contents of `.apm/`, and `apm compile -t <subset>` produces the project-root `AGENTS.md` for Codex / opencode from the same inputs. **Any** change to `apm.yml` or anything under `.apm/` invalidates both outputs. So this skill makes all file changes first (steps 1–5) and runs `apm install` (and `apm compile` where needed) exactly once at the end (step 6). Don't run install or compile partway through — later steps may add or modify files that must land in the same regeneration. If you ever edit `apm.yml` or `.apm/*` outside the prescribed order, you must re-run both before reporting back.

## 1. Pick an `apm` invocation

`apm` does not need to be installed — run it through `uvx`. Try in order, stopping at the first that works:

1. `uvx --from apm-cli apm --version`
2. `nix shell nixpkgs#uv -c uvx --from apm-cli apm --version`

If neither works (no `uvx` and no `nix`), tell the user to install one of [`uv`](https://docs.astral.sh/uv/) or [`nix`](https://nixos.asia/en/install) and stop. Don't try to install package managers yourself.

Use whichever prefix succeeded as the `apm` invocation for every subsequent `apm` call in this run (e.g., `uvx --from apm-cli apm install`).

## 2. Detect the host targets

The host targets go into `apm.yml` (next step) — `apm install` reads them, no `-t` flag needed. Detect from what's already on disk and the host you're running in:

- `.claude/` exists, or you're running in Claude Code → `claude`
- `.opencode/` exists, or you're running in opencode → `opencode`
- `.codex/` exists, or you're running in Codex → `codex`

Multiple matches are fine — declare all of them. If nothing matches and the host you're in isn't one of the three, use `AskUserQuestion` to confirm. Do **not** guess silently — installing for the wrong target wastes a round trip.

**Single vs. multiple targets:** `apm` has a bug where the plural `targets:` list breaks when only one entry is present. Use the singular `target: <name>` scalar for exactly one target, and the `targets:` list only when you have two or more.

## 3. Create or extend `apm.yml`

Before editing, note whether `srid/agency` is already listed under `dependencies.apm:` — step 6 needs that fact to decide whether to refresh the dep.

If `apm.yml` does not exist, write:

```yaml
name: <repo-directory-name>
version: 1.0.0
type: hybrid

target: <detected-target>

dependencies:
  apm:
    - srid/agency#master
```

(If you detected two or more hosts, use the `targets:` list form instead — see the note in step 2.)

If `apm.yml` already exists, edit it idempotently:

- If `dependencies.apm:` is missing the `srid/agency` entry, append `srid/agency#master`. Preserve every existing entry. If the `dependencies.apm:` block itself is missing, add it.
- If neither `target:` nor `targets:` includes the detected host, add it. Don't remove existing targets. When adding a host pushes the count from one to two, convert `target: <name>` into a `targets:` list with both entries; when removing a host (not something this skill does, but worth knowing) drops the count back to one, convert the list back to the scalar form.

Don't touch unrelated entries. Refreshing an existing `srid/agency` pin is handled by `apm deps update` in step 6 — don't hand-edit the ref here.

## 4. Ensure `.gitignore` covers agency runtime artifacts

`apm install` (which runs in step 6) will add `apm_modules/` for you, but `do` writes `.do-results.json` at the repo root during a workflow run and that should not be committed. Make sure both lines exist in `.gitignore` (create the file if missing), idempotently — don't duplicate entries that are already there:

- `/.do-results.json`
- `/apm_modules/` (verify; `apm install` may already have added it as `apm_modules/` — either form is fine)

## 5. Draft `.apm/instructions/workflow.instructions.md`

If this file already exists, **leave it alone** — the user has either already configured it or is intentionally hand-maintaining it. Skip to step 6.

If it's missing (whether this is a first-time setup or an upgrade where the user added `srid/agency` to `apm.yml` themselves but never wrote workflow instructions), create it now.

`do` runs autonomously but needs to know your project's check, format, test, and CI commands. Inspect the project to figure them out — look at:

- `package.json` `scripts:` (Node)
- `justfile` (just)
- `Makefile`
- `Cargo.toml`, `flake.nix`, `pyproject.toml`
- `.github/workflows/` for CI hints

For each of the four sections (Check, Format, Test, CI), there are three possible outcomes:

- **Found a clear command** in the project → fill it in.
- **Found a plausible command but you're not certain** → use `AskUserQuestion` to confirm. Offer the candidate as one option and "skip this section" as another, with a free-form fallback for the user to type a different command.
- **Found nothing** → use `AskUserQuestion` to ask the user directly. Always include a "skip this section" option so they can explicitly discard it. Don't fabricate commands.

Sections the user discards are **omitted from the generated file entirely** — no `# TODO` placeholders. `do` already handles missing sections by skipping the corresponding step with a note, which is the right behavior for a section the user has consciously declined.

Final file uses this template, including only the sections the user kept:

```markdown
---
description: Workflow commands for the do pipeline
applyTo: "**"
---

## Check command
<command>

## Format command
<command>

## Test command
<command>

## CI command
<command>

## Documentation
Keep `README.md` in sync with user-facing changes.
```

## 6. Refresh `srid/agency` (if already present), then run `apm install` (and `apm compile` for Codex / opencode)

If `srid/agency` was already listed in `apm.yml` at the start of this run (you noted this in step 3), first run `<apm-invocation> deps update srid/agency` from the directory containing `apm.yml`. `apm install` alone won't move an already-installed dependency to a newer ref — `deps update` is what pulls the latest commit on the pinned ref. Skip this sub-step on first-time setup, where step 3 just added the entry; `apm install` will fetch it fresh.

Then, regenerate the host configs in a single pass. Run `<apm-invocation> install` from the directory containing `apm.yml`. `apm` reads `target:` / `targets:` from the yml and generates the runtime-specific folders (`.claude/` / `.opencode/` / `.codex/`), plus adds `apm_modules/` to `.gitignore`.

**`install` does not generate the project-root `AGENTS.md` instruction file.** Codex and opencode read `AGENTS.md` at session start; without it they will not see the workflow instructions, code-police rules, or any other `.apm/instructions/` content. To produce it, also run:

```sh
<apm-invocation> compile -t <subset>
```

…where `<subset>` is the comma-separated list of `codex` and/or `opencode` from your declared targets (e.g., `-t codex,opencode` if both are declared, `-t codex` if only Codex). **Skip the compile step entirely if `claude` is the only target** — Claude Code reads `.claude/` natively and doesn't need `AGENTS.md` (compiling `CLAUDE.md` is intentionally avoided).

If `install` or `compile` fails, surface the error verbatim and stop — don't paper over it.

If you discover after this step that you still need to touch `apm.yml` or anything under `.apm/`, run `install` (and `compile` if applicable) again before moving on. The invariant at the top is non-negotiable.

## 7. Report back

Summarize for the user, in this order:

1. Which `apm` invocation you used (so the user knows the exact command for ad-hoc `apm` calls later).
2. Which target(s) ended up in `apm.yml` (and which form — `target:` scalar or `targets:` list).
3. Which workflow sections were filled in (and from where) versus skipped at the user's request.
4. Files changed (staged, not committed). Tell them to review the diff before committing.
5. **Optional instructions to consider adding** — list whichever of these files do **not** yet exist under `.apm/instructions/`, and explain briefly what each is for. They're project-specific and can't be auto-generated, but the user should know they exist so they can layer them on:
   - `code-police-rules.instructions.md` — project-specific quality rules checked alongside the built-in `code-police` rules.
   - `hickey-catalog.instructions.md` — project-specific complecting patterns extending the Hickey Layer 4 catalog.
   - `lowy-volatilities.instructions.md` — project-declared areas of volatility used by the Lowy review pass.

   Point them at [Kolu's `.apm/instructions/`](https://github.com/juspay/kolu/tree/master/.apm/instructions) as a worked example. Skip files that already exist.
6. **Restart the agent CLI** (Claude Code, Codex, opencode, etc.) so it picks up the newly generated skills — without a restart, the new skills won't be available in the running session.
7. After restart, try `talk` or `do` to verify everything works. Tell the user the **exact** invocation syntax for the target(s) you installed for — don't make them guess:
   - **Claude Code** → `/talk <question>` and `/do <task>` (slash commands).
   - **Codex** → `$talk <question>` and `$do <task>` (dollar prefix).
   - **opencode** → invoke `/skills` and pick `talk` or `do` from the list.

   If you installed for multiple targets, list the syntax for each.
