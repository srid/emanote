---
name: lowy
description: Evaluate architecture and module boundaries for volatility-based decomposition using Juval Lowy's framework (from "Righting Software", building on Parnas 1972). Use when reviewing module splits, service boundaries, new abstractions, or any decomposition decision. Trigger on phrases like "where should this boundary be", "how to split this", "module boundaries", "encapsulate change", "volatility", or references to Lowy, Parnas, or "Righting Software". Complements /hickey (interleaved concerns) with a different lens (change encapsulation).
context: fork
agent: Explore
---

# Lowy: Volatility-Based Decomposition Review

Evaluate module boundaries and decomposition decisions using Juval Lowy's volatility-based decomposition framework. The core question: **do your boundaries encapsulate axes of change, or do they just group related functionality?**

Source: Juval Lowy, [*Righting Software*](https://rightingsoftware.org/) (2019), building on David Parnas, ["On the Criteria to Be Used in Decomposing Systems into Modules"](https://www.win.tue.nl/~wstomv/edu/2ip30/references/criteria_for_modularization.pdf) (1972). See also: [Volatility-Based Decomposition](https://www.informit.com/articles/article.aspx?p=2995357&seqNum=2) (book excerpt).

## Key Idea

**Functional decomposition** groups code by what it does (UserService, PaymentController, AuthModule). **Volatility-based decomposition** groups code by what is likely to *change* — and encapsulates each axis of change behind a stable interface.

> "This principle governs the design of all practical systems, from houses, to laptops, to jumbo planes, to your own body. To survive and thrive they all encapsulate the volatility of their constituent components." — Lowy

Lowy's electricity analogy: a house's power supply has enormous volatility (AC/DC, 110v/220v, 50/60Hz, solar/grid/generator, wire gauges). All of it is encapsulated behind a receptacle. Without that encapsulation, you'd need an oscilloscope every time you plugged something in. The receptacle is the stable interface; the volatility behind it can change without affecting consumers.

**Functional decomposition maximizes the blast radius of change.** When boundaries track functionality rather than volatility, a single change cuts across multiple modules. "Functional decomposition is as diverse as the required functionality across all customers and points in time. The resulting huge diversity in the architecture leads directly to out-of-control complexity." Volatility-based decomposition contains the grenade in the vault.

### Two Types of Volatility in Business Logic

Lowy identifies two independent axes within any workflow:

1. **Sequence volatility** — the *order* of steps in a workflow can change independently of what those steps do. Different customers or use cases may require different orchestrations of the same activities. This volatility belongs in orchestrators (Lowy's "Managers").

2. **Activity volatility** — *how* a specific activity is performed can change independently of the sequence it appears in. There may be an unknown number of ways to do the same activity (different algorithms, providers, strategies). This volatility belongs in strategy components (Lowy's "Engines").

Conflating these two — putting orchestration logic and activity logic in the same module — means a change to either axis ripples into the other.

### Variable vs. Volatile

Not everything that varies is volatile. Lowy makes a critical distinction: adding an attribute to a data model is *variable* but not *volatile* — the architecture won't suffer. **"If you cannot clearly state what the volatility is, why it is volatile, and what risk the volatility poses in terms of likelihood and effect, then you need to look further."** Decomposing around things that merely vary (rather than things that are genuinely volatile) produces over-engineered boundaries that add cost without containing real change.

## The Evaluation

For every module boundary, service split, or new abstraction in the code under review:

### 1. Name the Volatility

What is likely to change behind this boundary? Be specific — not "requirements might change" but "the payment provider, the auth protocol, the notification channel." If you can't name concrete axes of change, the boundary may be arbitrary.

**Consider project-declared areas of volatility.** If the project has enumerated its own areas of volatility — the term of art Lowy uses throughout *Righting Software* — those declarations surface as system-reminders when you read a matching file (Claude Code's `paths:`-scoped rule mechanism; it is wiring, not Lowy doctrine). The schema, loosely modeled on Lowy's TradeMe enumeration (*Righting Software*, Ch. 5), is:

| Area of volatility | What changes | Why volatile (likelihood × effect) | Expected encapsulation |
|--------------------|--------------|------------------------------------|------------------------|

Rows in that table are **surviving candidates** after the project's own variable-vs-volatile screen (see §"Variable vs. Volatile" above). They are not findings, and they are not above review. Do two things with each row: (a) re-apply Lowy's bar — _"state what the volatility is, why it is volatile, and what risk the volatility poses in terms of likelihood and effect"_ — and challenge any row that fails it (Lowy: _"It is important to discuss the volatility candidates this way and even challenge them"_); (b) audit whether the boundaries under review actually encapsulate the surviving volatilities in a single component, rather than spraying or leaking them across modules.

**Speculative volatility is not volatility.** A change scenario counts only if it has happened before, is on a roadmap, or is a near-certain consequence of the domain (e.g. "payment providers change" in e-commerce). "What if we swap color spaces" in an app that has never swapped color spaces is speculation, not an axis of change. Lowy's framework is about *observed* or *plausible* volatility — designing for hypothetical change is over-engineering, not encapsulation.

**Weak volatility may not deserve its own boundary.** Some volatilities are real but too minor to justify a separate component. Lowy's example: notification delivery might be volatile, but if the system already has a message bus utility, a dedicated NotificationManager adds complexity without containing meaningful additional change. Ask: does this volatility justify the cost of an additional boundary, or can it be folded into an existing one?

### 2. Classify the Volatility

Is the volatility about *sequence* (the order/orchestration of a workflow) or *activity* (how a specific step is performed)? These are independent axes and should be encapsulated separately. A module that mixes both will be modified for two unrelated reasons.

Also check for *domain decomposition* — boundaries drawn around domain entities (ProjectService, TradesmanModule, AccountsManager) rather than around axes of change. Domain decomposition is functional decomposition wearing a domain hat. Lowy warns: it creates ambiguity about who does what and when, duplicates functionality across domain lines, and is nearly impossible to validate against use cases.

### 3. Functional vs. Volatility Boundary

Does this boundary exist because the code *does something different* (functional), or because what's behind it *changes independently* (volatility)? Functional boundaries look clean on day one but fracture under change. A `UserService` that groups all user operations is functional decomposition — the volatility of auth, profile data, and notification preferences are unrelated axes of change jammed behind one boundary.

**The naming test.** Lowy uses naming conventions as a diagnostic. Orchestrator names should be nouns associated with the encapsulated volatility (AccountManager, MarketManager — good; BillingManager — bad, the gerund "billing" signals functional grouping around an activity). Strategy/engine names should indicate the volatile activity (SearchEngine, TransformationEngine — good; AccountEngine — bad, no indication of what activity varies). If you struggle to name the component after a volatility axis, it may not encapsulate one.

### 4. Change Blast Radius

For a plausible change scenario (new provider, new format, new rule), trace how many modules would need to be modified. If the change leaks across boundaries, the decomposition is functional, not volatility-based.

**Volatility should decrease downward.** In a layered system, higher layers (clients, UI) should be the most volatile, and lower layers (data access, infrastructure) should be the least volatile. "The components in the lower layers have more items that depend on them. If the components you depend upon the most are also the most volatile, your system will implode." If high volatility lives deep in the stack, the blast radius of change is maximized.

**Check symmetry.** All good architectures are symmetric — you should see the same calling patterns across similar modules. If three of four workflows publish events but the fourth doesn't, or only one module has a particular coupling pattern, that asymmetry is a red flag for functional decomposition or a missed volatility axis. Symmetry can also be broken by the *presence* of something, not just its absence.

### 5. Interface Stability

Is the interface between modules stable under the changes the module encapsulates? The receptacle doesn't change when you switch from grid to solar. If the interface must change when the encapsulated volatility changes, the abstraction is leaking.

**Expose atomic business verbs, not implementation operations.** Lowy's key interface design principle: interfaces should expose indivisible business-level operations (credit, debit, transfer) rather than CRUD or implementation details. "Atomic business verbs are practically immutable because they relate strongly to the nature of the business which hardly ever changes." An interface that exposes `OpenPort()`, `ClosePort()`, `AdjustBeam()` alongside `ReadCode()` is mixing communication volatility with reading volatility — two axes jammed behind one interface.

**Good interfaces are reusable; implementations never are.** Lowy's "tool-hand" analogy: a stone axe and a computer mouse have nothing in common internally, but both reuse the same hand interface. If an interface can only be used by one consumer, it may be shaped around the implementation (functional) rather than around a stable contract (volatility-based). Well-designed contracts are logically consistent (operations form a coherent unit), cohesive (all aspects required, no more, no less), and independent (each stands alone).

### 6. Reuse Signal

Volatility-based building blocks are reusable because they encapsulate one axis of change. If a module can only be used in one context, it may be encapsulating functionality rather than volatility.

Lowy observes that reuse increases downward through layers: infrastructure and data-access components should be highly reusable across contexts, business-logic orchestrators are reusable across multiple clients, and clients/UI are rarely reusable. If a lower-layer component is locked to a single consumer, the boundary likely tracks functionality rather than a genuine axis of change.

### 7. The Almost-Expendable Test

Lowy's litmus test for correct decomposition: when a change request arrives, the response should be *contemplative* — you think through how to adapt. If a module is *expensive* to change, it's too big (functional decomposition has coupled unrelated concerns). If a module is *expendable* (trivially thrown away), it's an unnecessary boundary. If a module is *almost expendable* — it encapsulates just enough to contain one axis of change, and replacing it is straightforward but not trivial — the decomposition is correct.

## Fact-Check Your Own Evaluation

After completing all steps, **invoke `/fact-check` on your own output**. The fact-check catches:

- Findings you talked yourself out of ("However, this is a reasonable grouping..." / "acceptable for now")
- Functional boundaries rationalized as volatility boundaries without naming the concrete axis of change
- "Low blast radius" used as a synonym for "ignore"
- Change scenarios you didn't actually trace through the code
- Domain decomposition dressed up as volatility decomposition

**Flag these phrase shapes** — they mean you stopped one step early:

- _"This boundary groups related functionality but could also be seen as encapsulating volatility"_ — name the volatility or it's functional decomposition. "Could be seen as" is not an axis of change.
- _"The interface would only need minor changes"_ — minor interface changes are still leaking. The receptacle doesn't change at all.
- _"This module is only used in one place, but that's fine for now"_ — single-use is the reuse signal firing. Investigate.
- _"The boundary follows the framework's conventions"_ — framework conventions are functional decomposition by default. Convention is not volatility analysis.
- _"This could theoretically change independently"_ — theoretical independence without a concrete change scenario is wishful thinking.
- _"Out of scope for this PR" / "pre-existing"_ — process judgment, not a volatility judgment. Defer with an issue link or fix it.
- _"The module encapsulates [domain entity]"_ — domain entities are not volatility axes. What *about* the entity changes? Name the specific volatility or it's domain decomposition.
- _"This is variable, so we should encapsulate it"_ — variable is not volatile. Can you state the risk in terms of likelihood and effect?

If fact-check finds issues, revise before presenting to the user.

## Output Format

1. **Boundaries examined** — List each module boundary or decomposition decision reviewed.
2. **Volatility map** — For each boundary: what volatility it encapsulates (or fails to), classified as sequence or activity volatility where applicable.
3. **Findings** — Boundaries that track functionality rather than volatility, with blast-radius analysis. Include symmetry violations and layering inversions.
4. **Simplifications** — Concrete restructuring to align boundaries with axes of change.
5. **Fact-check result** — Output of `/fact-check` on this evaluation, including the phrase-shape check.
6. **Actions** — One entry per finding, formatted so a downstream step (e.g. `/do`'s PR comment composer) can lift each entry into a table row. Each entry **starts with a short bolded finding label (≤8 words)** naming *what* is wrong, then dispositions it as **Fix in this PR** or **Defer `#<issue>`**. Every finding must appear here — including those labeled "pre-existing" or "orthogonal". A finding that never reaches this section has been dismissed, not deferred.

Example: `**useViewport encapsulates ghost concern** — Fix in this PR: delete the hook, let FitAddon measure per-tile.`

No findings → "No actions." Findings without actions = incomplete review.

## Relationship to /hickey

This skill and `/hickey` are complementary lenses. Hickey asks "are independent concerns interleaved?" Lowy asks "do boundaries encapsulate axes of change?" Run both on architectural decisions for full coverage.

### When Hickey and Lowy Disagree

The two lenses can produce conflicting recommendations. Lowy may say "merge these — shared volatility is duplicated across both" while Hickey says "keep them separate — a mode flag would complect configuration with implementation." Neither lens is wrong; they're optimizing for different things.

The resolution pattern: **unify the volatile axis without complecting the strategies.** Typically this means a wrapper or shared module that encapsulates the volatile part (satisfying Lowy) while the distinct strategies remain private and uncomplected (satisfying Hickey). If merging for blast-radius reduction requires a mode flag, conditional branching, or type-switching — that's complecting. Find the layer where unification is mechanical (a shared function, a common interface, a single config source) rather than conditional.
