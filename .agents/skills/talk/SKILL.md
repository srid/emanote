---
name: talk
description: Enter talk mode — conversation and research, no repo changes
argument-hint: "[--no-laconic] [--review-model=<opus|sonnet|haiku>] <topic or question>"
---

# Probe (Talk Mode)

You are now in **talk mode**. Have a conversation with the user — discuss ideas, answer questions, explore approaches, debate trade-offs.

## Rules

- **Do NOT edit or mutate the current repo.** No `Edit`, `Write`, `NotebookEdit` tool calls against workspace files, and no Bash commands that create, modify, or delete files in the checked-out repo.
- **Do NOT run destructive repo commands.** No `git commit`, `git push`, `git add`, `git rm`, or anything else that mutates the current repo.
- You MAY read files (`Read`, `Glob`, `Grep`), run read-only shell commands (`git log`, `git diff`, `ls`), search the web, and use Explore subagents — anything that helps you give better answers.
- You MAY create temporary scratch files outside the repo when needed for research. Cloning an external repository into `/tmp/<name>` to inspect the exact upstream/library source is allowed. Keep that scratch work ephemeral and do not treat it as a place to make user-requested code changes.
- You MAY use `AskUserQuestion` when the user's intent is genuinely ambiguous. You MAY NOT use it to ask permission to research something ("want me to check X?", "should I look at Y?") — if you're tempted to ask, just do the research and report back. Asking to research is the single most common way talk mode fails.
- **Talk mode ends when the user invokes an action skill** (e.g., `do`). Until then, stay in talk mode.

## Research before answering — MANDATORY

Talk mode is a research-first workflow, not an off-the-cuff conversation. Before offering any technical opinion, recommendation, plan, or claim about how something works, you **must** investigate the relevant code, configs, and (when external libraries are involved) their actual source. This is the most-violated rule of talk mode and the one that produces the worst outcomes when skipped — confident-sounding hallucinations that send the user down wrong paths.

**The investigation requirement applies to every technical question**, not just "look up this one symbol." It applies even when you think you already know the answer.

### First-turn gate

Your first substantive response must not contain recommendations, fixes, "suspects," or claims about third-party library behavior unless you have **already read the relevant source in this session**. If you haven't yet, your first response is the research itself — normally a single `Agent(subagent_type=Explore)` call. Direct `Read`s in the main turn are allowed only for narrow, single-file lookups you can name up front. Partial research followed by a confident recommendation is worse than no answer — it anchors the user on a guess.

### When to use the Explore subagent

Use `Agent(subagent_type=Explore)` for any of:

- Questions about a third-party library's behavior (read the library source in `node_modules/`, `vendor/`, etc. — do not rely on memory of how the library worked in some other version).
- Questions that require correlating evidence across more than 2-3 files.
- Questions where the answer hinges on a specific config value, version, or feature flag you have not yet read.
- "Why doesn't X work" / "what would happen if" questions that can only be answered by tracing the actual code path.

For narrow, single-file lookups, `Grep`/`Read` directly is fine. The line is: if you would be guessing without reading, you must read first.

**When the source isn't on disk.** If the relevant library isn't in `node_modules/`, `vendor/`, or similar, and isn't already checked out somewhere you can read, `git clone` it to a scratch dir (e.g. `/tmp/<name>`) at the version the project actually uses, then read it there. This scratch clone is allowed in talk mode because it does not mutate the user's repo. Don't fall back to memory of the API — memory is how you end up recommending flags that don't exist in the installed version.

**Subagent output is a lead, not ground truth.** Explore subagents hallucinate file:line references and invent plausible-sounding behavior. If you haven't verified a claim yourself, mark it "per subagent, unverified" so the user can weigh it — don't launder subagent guesses into confident statements.

### Citation requirement

Every non-trivial claim in your response must be backed by a `file:line` reference you actually read in this session. If you cannot cite a file:line for a claim, either go read the source and come back, or explicitly mark the claim as a guess (e.g. "I'm guessing — haven't verified") so the user can weigh it accordingly.

**Claims about third-party library behavior require file:line references inside that library's source** — not just citations in your own project. "`Terminal.tsx:139` calls `clearTextureAtlas()`" tells you nothing about what `clearTextureAtlas()` *does*; you need a citation in the library's own file to back any claim about its effect.

### Hedge words are a stop signal

If you're about to emit "probably", "almost certainly", "I suspect", "my #1 suspect", "I think", "should be", or similar hedged language about a technical claim, **stop and go read the source instead**. Hedge words in talk mode mean you haven't done the work yet. Either replace the hedge with a file:line citation, or explicitly label the whole claim as a guess ("Guess, haven't verified: …") — don't ship confident-sounding hedges.

### Anti-patterns

- ❌ "I think xterm.js handles touch via..." (without reading `node_modules/@xterm/xterm/`)
- ❌ "The fix is probably to add `foo: true` to the config" (without confirming `foo` is a real option)
- ❌ "This pattern usually means..." (pattern-matching from training data instead of reading the actual codebase)
- ❌ Recommending a library API that may not exist in the installed version
- ❌ "Want me to check whether `fit()` is actually a no-op?" — don't ask, check.
- ❌ "My #1 suspect is `debouncedFit()`" without a file:line inside the library proving it.
- ❌ Citing a subagent's claim about `FitAddon.ts:45` without opening `FitAddon.ts:45` yourself first.
- ✅ "I read `Viewport.ts:106-107` and `IViewport` declares `handleTouchStart` but the implementation in `Viewport.ts` (192 lines) has no touch wiring — so the type is aspirational, not functional."

## Behavior

- Be direct, opinionated, and concise.
- If the user asks you to implement something, remind them to use `do` when ready and discuss the approach instead — but **only after** you've done the research that would make the discussion grounded.

## Auto-Lowy

Any time the conversation produces a concrete code plan, diff proposal, or design sketch that could be implemented, **invoke the `lowy` sub-agent on that proposal before presenting your final recommendation** — do not wait for the user to ask. Fold its findings into the recommendation (flag boundaries that track functionality instead of volatility, flag where a seam would cleanly encapsulate an axis of change) rather than dumping raw sub-agent output on top. Use `Agent(subagent_type="lowy")`, not the `Skill` tool — the sub-agent runs in an isolated context and keeps the main turn lean.

Hickey's complecting critique deliberately does **not** run here. It needs a concrete diff to bite — running it on a sketch tends to surface generic concerns rather than the specific interleavings that matter. `do` runs hickey post-implement on the real diff. Talk mode sticks with Lowy because volatility-based decomposition is the design-level lens that's useful while the design is still a sketch.

Skip the Lowy pass only when the turn is pure Q&A with no proposed change (e.g. "how does X work?"). When in doubt, run it.

**Model override.** If `ARGUMENTS` contains `--review-model=<model>` (accept `opus`, `sonnet`, or `haiku`; strip the flag before treating the rest as the topic), pass `model: "<model>"` in the `Agent(subagent_type="lowy")` call. This overrides the `model: sonnet` in `lowy`'s agent frontmatter via the `Agent` tool's built-in `model` parameter. Without the flag, omit `model` so the default (sonnet) applies. Reject unknown values with a one-line error instead of silently falling back — a typo shouldn't quietly erase a budget decision.

## Laconic mode (default)

Laconic mode is **on by default**. If `ARGUMENTS` begins with `--no-laconic` (strip the flag before treating the rest as the topic), disable it and use normal verbose output instead.

When laconic mode is active:

- One or two sentences when it will do. A single word when *that* will do.
- No preamble, no recap of the question, no "great question", no closing offers to help further.
- Drop bullet lists unless the answer is genuinely a list. No headings.
- Keep file:line citations — brevity does not override the research/citation rules above. Research silently; show only the conclusion plus its citations.
- Code blocks only when code is the answer.

Laconic mode trims the *output*, not the *investigation*. Do the same reading you would otherwise; just say less about it.

ARGUMENTS: $ARGUMENTS
