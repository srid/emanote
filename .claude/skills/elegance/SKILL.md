---
name: elegance
description: Iteratively study and apply elegant coding patterns. Each iteration - understand the code, research what simple and elegant code looks like, apply learnings, verify with CI. Use as a standalone refactoring pass or when the user asks to make code more elegant, simple, or idiomatic.
---

# Elegance

Iteratively study and apply elegant coding patterns. Each iteration: understand the code, research what simple & elegant code looks like, apply learnings, verify with CI.

Run for **3 iterations** (or as specified by the user — can be a number or a duration like `2h`).

## 0. Determine Scope

- Before starting, use the `AskUserQuestion` tool to ask: should this operate on the **whole codebase** or only on **changes in the current branch/PR**?
- If scoped to current branch/PR, use `git diff main...HEAD` (or the appropriate base branch) to identify changed files and limit all subsequent steps to those files only.

## For each iteration (1 to N):

### 1. Understand

- Read through the relevant source files.
- Note patterns, repetition, unnecessary complexity, non-idiomatic code.

### 2. Research

- Use WebSearch/WebFetch to research what simple, elegant (yet readable!) code looks like for the relevant technology.
- Look for idiomatic patterns, standard simplifications, and community best practices.
- Focus on: simplicity, readability, removing unnecessary abstraction, leveraging language features.

### 3. Apply

- Refactor based on what you learned.
- This can include: edits, code reorganization, or even rewrites where simplicity demands it.
- Prefer fewer lines, clearer intent, and idiomatic style.
- Don't add abstractions — remove them.

### 4. Verify

- Run tests/CI to check edits.
- If CI fails, fix the issues before proceeding to the next iteration.

### 5. Log Progress

- Briefly note what changed in this iteration and why.

## After all iterations

- Do **not** git commit. Leave all changes in the working directory for the user to review.
- Present a summary of what changed across all iterations.

## Principles

- **Simple over clever**: Elegant code is simple code.
- **Readable over terse**: Brevity is good, but not at the cost of clarity.
- **Idiomatic over generic**: Use the language's strengths. Write TypeScript like TypeScript, not like Java.
- **Each iteration builds on the last**: Don't undo previous improvements. Deepen them.
