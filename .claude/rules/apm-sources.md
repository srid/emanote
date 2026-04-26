---
paths:
  - ".claude/**,.opencode/**"
---

## Generated Files — Do Not Edit Directly

> **This rule only applies if an `.apm/` directory exists somewhere in the project.** If there is no `.apm/` directory at any level, `.claude/` and `.opencode/` files are vendored directly and can be edited in place. To locate it, search for a directory matching `**/.apm/` from the project root.

Everything under `.claude/` and `.opencode/` is **generated** from `.apm/` sources by APM. Direct edits will be overwritten on the next `apm install` run.

To modify agent configuration, find the `.apm/` directory (it may be at the project root or nested under a subdirectory such as `agents/.apm/`), edit the source files there, then run `apm install` to regenerate.

**For Codex / opencode targets, also run `apm compile -t codex,opencode`** (or whichever subset applies) — `install` regenerates the runtime folders but does not produce the project-root `AGENTS.md` that those hosts read. Claude reads `.claude/` natively, so no compile step is needed when `claude` is the only target.
