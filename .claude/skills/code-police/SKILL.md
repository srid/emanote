---
name: code-police
description: Review code for quality, simplicity, and common mistakes before declaring work complete.
---

# Code Police

Review the current changes (scoped to the current branch/PR) against the rules below **and any additional code-police rules from project instructions**, then run three passes in order.

## Rules

### dry-rule-of-three

Two similar instances are fine — don't abstract prematurely. Three is the threshold for extraction. But identical content that must stay in sync (same HTML, same version string) should be deduplicated immediately regardless of count. Versions, ports, paths — define once, reference everywhere.

### invalid-states-unrepresentable

Use discriminated unions, not booleans or stringly-typed fields. If two fields can't both be `undefined` at the same time, model that in the type.

### no-dead-code

Aggressively remove unused code. No commented-out blocks, no "just in case" leftovers.

### no-silent-error-swallowing

Never silently swallow errors. Empty `catch {}` blocks, bare `catch: pass`, and `|| true` hide failures. At minimum, log the error. If the catch is intentional (best-effort operation), add a comment explaining _why_ the error is safe to ignore.
_Rationale_: Silent swallowing masks bugs — failures disappear without a trace, making debugging impossible.

### comments-why-not-what

Add comments where the _why_ isn't obvious from the code. Don't comment the _what_. Also comment where the _what_ isn't obvious — non-obvious guards, CSS workarounds, platform-specific behavior. Non-obvious workarounds (temp files, wrapper scripts, env var shims) must have a comment explaining why they exist.

## Pass 1: Rule checklist

Present a table with **every rule above**:

| Rule ID | Violation found? | What was identified | Action taken |
| ------- | ---------------- | ------------------- | ------------ |

If no violation was found for a rule, mark it as "No" with a brief note on what was checked. Every rule must appear in the table — no skipping.

## Pass 2: Fact-check

Audit the changes for **correctness and rigor**. This is not a style review — it's a logic review. Find places where the code lies to itself.

Flag:

- **Silent error swallowing** — bare `try/catch: pass`, empty `catch {}`, `|| true`, errors caught but not propagated, `Result`/`Option` silently defaulted.
- **Inaccurate fallbacks** — defaults masking misconfiguration, "sensible defaults" that aren't sensible for the failure case, fallback paths that silently degrade correctness.
- **Wishful thinking** — assumptions about input shape without validation at boundaries, code that "can't fail" but actually can, race conditions papered over with comments.
- **Logic errors** — always-true/false conditions, off-by-one, wrong operators, shadowed variables.

For each finding: file, line, one-line risk, concrete fix. If no issues, say so — don't invent problems.

**Anti-patterns in YOUR review (strictly banned):**

- NEVER talk yourself out of a finding. If you identified a problem, it IS a problem. No "However..." or "acceptable tradeoff."
- NEVER use "theoretically X but practically Y" to dismiss fragility.
- NEVER issue "no action needed" on a finding you just described.
- NEVER end with reassurance. No "the logic is sound" unless you genuinely found zero issues.
- Assume the code is wrong until proven right. You are a prosecutor, not a defense attorney.

## Pass 3: Elegance

Review the changes for elegance and simplicity. For each iteration (run 3 iterations):

1. **Understand** — Read through the changed files. Note patterns, repetition, unnecessary complexity, non-idiomatic code.
2. **Research** — Use WebSearch/WebFetch to research what simple, elegant (yet readable!) code looks like for the relevant technology.
3. **Apply** — Refactor based on what you learned. Prefer fewer lines, clearer intent, idiomatic style. Don't add abstractions — remove them.
4. **Verify** — Run tests/CI to check edits.

Principles:

- **Simple over clever**: Elegant code is simple code.
- **Readable over terse**: Brevity is good, but not at the cost of clarity.
- **Idiomatic over generic**: Use the language's strengths.

## Output

After all three passes, present a combined summary:

| Pass       | Issues found | Details                  |
| ---------- | ------------ | ------------------------ |
| Rules      | N            | Brief summary or "Clean" |
| Fact-check | N            | Brief summary or "Clean" |
| Elegance   | N            | Brief summary or "Clean" |

If ANY pass found issues, clearly state: **"Violations or issues found"** so the workflow can route to a fix step.

If all passes are clean, state: **"All clear"**.

## Additional principles

### Simple, not easy (Rich Hickey)

Simple means _not interleaved_. Each module does one thing. Data flows through arguments and return values, not shared mutable state or indirection.

- No unnecessary abstractions. If a thing has one implementor, it doesn't need an interface/base class.
- No "for future use" code. Build what's needed now.
- Prefer plain data over objects with behavior.

### Completeness

- Implement the full spec. Read the plan/requirements and check every deliverable.
- Run CI locally before declaring done.
- Run tests.

### Justfile

- Every recipe must have a doc comment (line starting with `#` above the recipe name).

### Module structure — volatility-based decomposition

Group code by _rate of change_, not by technical layer. Things that change together live together; things that change independently get separate modules.

- Each module should own one volatility zone.
- Shared constants used by multiple modules get their own file.

### Readability

- Every exported type and every component needs a doc comment.
- Avoid deeply nested callbacks. Extract into named functions.
