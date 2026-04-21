---
name: code-police
description: Review code for quality, simplicity, and common mistakes before declaring work complete.
---

# Code Police

Review the current changes (scoped to the current branch/PR) against the rules below **and any additional code-police rules from project instructions**, then run three passes in order.

## Rules

### dry-rule-of-three

Two similar instances are fine — don't abstract prematurely. Three is the threshold for extraction. But identical content that must stay in sync (same HTML, same version string) should be deduplicated immediately regardless of count. Versions, ports, paths — define once, reference everywhere.

### prefer-focused-library

Before hand-rolling a utility (string tokenizer, quoted-string parser, date helper, semver comparator, URL builder, CLI arg parser, tree walker, regex-based matcher, path normalizer, etc.), check whether a focused library solves the same problem. If one exists with a matching scope and a reasonable bundle cost, **prefer it — even if it's a new dependency**. Hand-rolling is only justified when the library would add capabilities you actively don't want (tagging, env expansion, i18n layers, etc. you'd have to ignore), or when the hand-roll is genuinely a handful of lines with no branching.

_Rationale_: "Zero deps" is an easiness judgment dressed as a simplicity judgment. Code you don't own is genuinely simpler than code you do own — it doesn't accumulate private test fixtures, it doesn't bitrot when requirements shift, and its edge cases are someone else's problem to fix. Hand-rolled utilities routinely grow past the library they displaced as new edge cases surface. A small focused library with a single exported function is the _same_ complexity to your reader as a one-line utility import, and _less_ complexity than a 40-line loop with state variables.

_How to apply_: When you're about to write a loop with nested state-machine variables, a tokenizer, a parser, a semver comparator, a date math helper, a quoted-string handler, or a path normalizer — **stop and search for the focused library first**. Only fall back to hand-roll after seeing a concrete library and judging that its scope genuinely exceeds what you need.

_Anti-patterns in this rule's application_:

- "It's already in the tree" is not a _requirement_ for preferring the library. Adding a small focused dep is fine. The "already transitive" check is a convenience shortcut, not a gate.
- "Only ~40 lines, well-scoped" is not a license to hand-roll. 40 lines of state-machine code is usually worse than one line of library call plus a dep.
- "I don't want a dep" is dependency-aversion, not simplicity. State it as such in the eval and evaluate it honestly as a preference, not as a neutral principle.
- The gating criteria are **scope fit** and **bundle cost** — not ideology in either direction. Left-pad exists; don't flip to "always use a library." Judge each case on whether the library's surface matches what you need.

### invalid-states-unrepresentable

Use discriminated unions, not booleans or stringly-typed fields. If two fields can't both be `undefined` at the same time, model that in the type.

### no-dead-code

Aggressively remove unused code. No commented-out blocks, no "just in case" leftovers.

### no-silent-error-swallowing

Never silently swallow errors. Empty `catch {}` blocks, bare `catch: pass`, and `|| true` hide failures. At minimum, log the error. If the catch is intentional (best-effort operation), add a comment explaining _why_ the error is safe to ignore.
_Rationale_: Silent swallowing masks bugs — failures disappear without a trace, making debugging impossible.

### no-unbounded-growth

Collections, buffers, and listeners that grow with usage must have a bound or a cleanup path. Common violations:

- **Unbounded arrays/lists** — pushed in a callback or event handler with no cap or eviction. Ask: _can this grow forever during a long-running session?_
- **Missing debounce on high-frequency sources** — `fs.watch`, `resize`, `scroll`, `mousemove`, WebSocket `onmessage`, or any event that can fire many times per second. Each invocation that does non-trivial work (I/O, parsing, DOM mutation, allocation) needs a debounce or throttle. A bare handler is only acceptable if the work is O(1) and allocation-free.
- **Large allocations in hot paths** — reading an entire file/stream into a single buffer when the consumer processes it incrementally. Prefer streaming/chunked reads when the data source can grow without bound.
- **Duplicated watchers/listeners** — N callers each installing their own watcher on the same resource instead of sharing one. Each duplicate multiplies callback cost and file-descriptor usage.

_Rationale_: LLM-generated code defaults to the simplest correct implementation, which is often O(n) in session lifetime. These patterns silently degrade performance over hours/days and surface as "the app got slow" with no obvious cause. The fix is almost always straightforward (cap, debounce, stream, share) but must be applied at write time — it's rarely caught in review because the code is functionally correct.

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
- **Slow leaks** — collections that grow without bound, event handlers doing heavy work on every fire without debounce, watchers/listeners registered per-caller instead of shared, buffers sized to the full input when streaming would work.

For each finding: file, line, one-line risk, concrete fix. If no issues, say so — don't invent problems.

**Principles:**

- **Fail loud over fail silent**: Code should scream when something is wrong, not quietly do the wrong thing.
- **Fallbacks must be justified**: Every default/fallback needs a reason why that value is correct for the failure case, not just convenient.
- **Precision over coverage**: Better to catch 3 real issues than flag 20 maybes.

**Anti-patterns in YOUR review (strictly banned):**

- NEVER talk yourself out of a finding. If you identified a problem, it IS a problem. No "However..." or "acceptable tradeoff."
- NEVER use "theoretically X but practically Y" to dismiss fragility.
- NEVER issue "no action needed" on a finding you just described.
- NEVER end with reassurance. No "the logic is sound" unless you genuinely found zero issues.
- Assume the code is wrong until proven right. You are a prosecutor, not a defense attorney.

## Pass 3: Elegance

Review the changes for elegance and simplicity.

**If running under Claude Code** (the `Skill` tool is available): invoke the bundled `/simplify` skill via the Skill tool. It runs three parallel lenses — reuse, quality, efficiency — over the current diff and applies fixes. Prefer this path.

**On other harnesses** (no `/simplify` available), run the inline loop instead. For each iteration (run 3 iterations):

1. **Understand** — Read through the changed files. Note patterns, repetition, unnecessary complexity, non-idiomatic code.
2. **Research** — Use WebSearch/WebFetch to research what simple, elegant (yet readable!) code looks like for the relevant technology.
3. **Apply** — Refactor based on what you learned. Prefer fewer lines, clearer intent, idiomatic style. Don't add abstractions — remove them.
4. **Verify** — Run tests/CI to check edits.

Principles:

- **Simple over clever**: Elegant code is simple code.
- **Readable over terse**: Brevity is good, but not at the cost of clarity.
- **Idiomatic over generic**: Use the language's strengths.
- **Each iteration builds on the last**: Don't undo previous improvements. Deepen them.

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
