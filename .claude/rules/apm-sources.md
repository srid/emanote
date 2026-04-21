---
paths:
  - ".claude/**,.opencode/**"
---

## Generated Files — Do Not Edit Directly

> **This rule only applies if an `.apm/` directory exists somewhere in the project.** If there is no `.apm/` directory at any level, `.claude/` and `.opencode/` files are vendored directly and can be edited in place. To locate it, search for a directory matching `**/.apm/` from the project root.

Everything under `.claude/` and `.opencode/` is **generated** from `.apm/` sources by APM. Direct edits will be overwritten on the next `apm install` run.

To modify agent configuration, find the `.apm/` directory (it may be at the project root or nested under a subdirectory such as `agents/.apm/`), edit the source files there, then run `apm install` to regenerate.
