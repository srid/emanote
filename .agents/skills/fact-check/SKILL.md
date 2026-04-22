---
name: fact-check
description: Audit code for correctness and rigor — logic errors, silent error swallowing, wishful thinking, and unjustified fallbacks. This is not a style review; it's a logic review. Use when you want a focused correctness audit separate from the full code-police pass.
argument-hint: "[branch|all]"
context: fork
agent: Explore
---

# Fact-Check

Audit code for **correctness and rigor**. This is not a style review — it's a logic review. Find places where the code lies to itself.

## 0. Determine Scope

Scope comes from `$ARGUMENTS`:

- `branch` (default when `$ARGUMENTS` is empty) — audit only changes in the current branch/PR. Use `git diff main...HEAD` (or the appropriate base branch) to identify changed files and limit all subsequent steps to those files.
- `all` — audit the whole codebase.
- Anything else — treat the argument as the target itself (a file path, a diff range like `origin/main...HEAD`, or inline text/output to audit, e.g. when invoked by `hickey` to audit its own evaluation). Limit the audit to that target.

Do **not** use `AskUserQuestion`. This skill runs in a fork and is routinely invoked autonomously (e.g. from `/do` via `hickey`).

## What to flag

### 1. Silent error swallowing
- Bare `try/except: pass`, empty `catch {}`, `|| true` hiding real failures.
- Errors caught and logged but not propagated when callers depend on failure signals.
- `Result`/`Option`/`Maybe` types silently defaulted without justification.

### 2. Inaccurate fallbacks
- Default values that mask misconfiguration (e.g., falling back to `""` or `null` when the real fix is to fail loud).
- "Sensible defaults" that aren't actually sensible for the failure case.
- Fallback paths that silently degrade correctness (e.g., returning stale data without indicating staleness).

### 3. Wishful thinking
- Assumptions about input shape/type without validation at system boundaries.
- Code that "can't fail" but actually can (network, filesystem, permissions).
- Race conditions papered over with comments like "this should be fine".

### 4. Logic errors
- Conditions that are always true/false.
- Off-by-one errors, wrong comparison operators.
- Variables shadowed or unused in a way that changes behavior.

### 5. Slow leaks
- Collections that grow without bound, event handlers doing heavy work on every fire without debounce, watchers/listeners registered per-caller instead of shared, buffers sized to the full input when streaming would work.

## Workflow

1. Read the diff (or full files if scoped to whole codebase).
2. For each changed file, read enough surrounding context to understand intent.
3. List every finding with file, line, and a one-line explanation of the risk.
4. For each finding, propose a concrete fix (code snippet or direction).
5. If no issues found, say so — don't invent problems.

## Principles

- **Fail loud over fail silent**: Code should scream when something is wrong, not quietly do the wrong thing.
- **No wishful thinking**: If it can fail, handle the failure explicitly.
- **Fallbacks must be justified**: Every default/fallback needs a reason why that value is correct for the failure case, not just convenient.
- **Precision over coverage**: Better to catch 3 real issues than flag 20 maybes.

## Anti-patterns in YOUR review (strictly banned)

You are an LLM reviewing code. LLMs have a strong bias toward declaring code "acceptable" to avoid conflict. This command exists precisely to counteract that. Follow these rules absolutely:

- **NEVER talk yourself out of a finding.** If you identified a problem, it IS a problem. Do not follow up with "However..." or "Verdict: acceptable tradeoff" or "practically safe." If the code has a bogus fallback, say so and propose a fix. Period.
- **NEVER use "theoretically X but practically Y" to dismiss.** "Theoretically fragile but practically safe" is exactly the kind of wishful thinking this command is supposed to catch. If it's fragile, flag it and fix it.
- **NEVER issue a verdict of "no action needed" on a finding you just described.** If it wasn't worth acting on, you shouldn't have listed it. Every finding you report MUST have a concrete fix.
- **NEVER end with reassurance.** No "the logic is sound", no "the approach correctly targets the root cause", no "no other issues found" unless you genuinely found zero issues. Your job is to find problems, not to make the author feel good.
- **Assume the code is wrong until proven right.** The default posture is skepticism, not charity. You are a prosecutor, not a defense attorney.
