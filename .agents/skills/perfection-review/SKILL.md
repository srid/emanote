---
name: perfection-review
description: >-
  Adversarial "perfection" review — hold a change to an *ideal* bar, not just a correct one,
  assuming eternal time, unlimited energy, and no ship pressure. Use when the user asks to
  review "for perfection", to make a defect "impossible to express", or to hunt where a defect
  "relocates" across review rounds. Grounds every claim in the diff, fans out adversarial
  verifiers via Workflow, and reports residual surfaces with a structural fix for each. ONLY
  invoke when the user explicitly asks for a perfection / ideal-bar review.
argument-hint: "[<pr-number>] [--base <branch>] [--post]"
---

# Perfection review

The bar is not **"closed"** but **"the defect can no longer be expressed."** Assume eternal
time, unlimited energy, no deadline. "Overridable", "acceptable for scope", and "documented
intent" are *still holes*.

Most defects are **one shape in costumes**. Name the shape, then hunt **where it relocates** —
a fix that satisfies the literal ask but lets the defect resurface one seam over is not done.
Track it across rounds until it has nowhere left to go.

## Method

1. **Ground in the diff, not the story** — the PR body, commit messages, docs, and the
   author's claims are assertions to falsify, not facts. Check them against the code.
2. **Letter vs effect** — a safety added but never exercised is inert. Require a real path
   that uses it and a test that fails when it is removed.
3. **Unspellable > absent** — make the wrong thing impossible to write, not merely
   discouraged. If a future author can still spell the defect, it isn't closed. Beware a
   guarantee that proves *presence*, not *behaviour*.
4. **Reconcile claim and code** — an overclaiming comment or doc is itself a defect: make the
   code earn the sentence, or soften the sentence.
5. **Finish the blast radius** — not done until every dependent and downstream is carried to
   the same bar and verified against the final state.

## Verify adversarially — use Workflow

**Review with fresh context, separate from the author** — a reviewer carrying the author's
context rationalizes the author's intent; a fresh mind grounded only in the diff does not.
Fan out grounded verifiers (one per claim) **plus an adversary whose only job is to express
the defect anyway**, each citing the diff; then synthesize. Default to *refuted-if-uncertain*,
loop until nothing new surfaces, and re-verify the headline finding yourself. Keep agent
schemas **flat** and isolate the adversary so one failure can't abort the run.

## Report

Lead with **credit** for what's closed — don't move goalposts on done work — then the
**residual surfaces**, ranked, each with its structural fix. Frame each finding as the
**invariant it violates**, not a lone exploit: a single proof-of-concept trains a one-line
patch and the defect just relocates — give the property, and if you must show an example,
give a few from different angles and call it one costume of many. Separate "the product is
fine" from "the claim isn't yet true". Post to the PR only when asked (`--post`).
