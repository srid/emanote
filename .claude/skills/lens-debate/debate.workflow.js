// The Workflow runtime requires `export const meta` to be the FIRST statement
// and a PURE LITERAL (no variable interpolation), so the primary model is
// inlined as 'opus' in the phase entries below. The only Apply-phase agent is a
// single `apply:all` on `model` (Opus) that implements and commits each agreed
// fix in-session. Those inlined 'opus' phase entries plus the `const MODEL`
// socket just after meta are the model bindings — every other model reference in
// this script reads MODEL lazily at input-resolution time, well after meta is
// evaluated.
export const meta = {
  name: 'lens-debate',
  description:
    'lowy + hickey review a diff independently in parallel, then debate every finding to consensus; apply the agreed fixes',
  phases: [
    { title: 'Review', detail: 'lowy and hickey (and optionally code-police) review the diff independently, in parallel', model: 'opus' },
    { title: 'Debate', detail: 'lowy and hickey cross-examine every finding until they agree per-finding', model: 'opus' },
    { title: 'Apply', detail: 'implement each agreed fix as its own commit (skipped under apply:false)', model: 'opus' },
  ],
}

// The model every lens/agent runs on. SKILL.md flags this as load-bearing
// (lenses run on Opus, overriding their `model: sonnet` frontmatter) and model
// migrations are a recurring change — keep it to one socket. Inlined into the
// phase entries above (meta must be a pure literal); the `model` input below
// defaults to it.
const MODEL = 'opus'

// ---------------------------------------------------------------------------
// Inputs (passed via the Workflow tool's `args`)
// ---------------------------------------------------------------------------
// The harness JSON-ENCODES `args` before the workflow sees it, so it arrives as a
// STRING even when the caller passed a real object; a bare `args.repoPath` would then
// be `undefined` and every input (repoPath/base/rationale/…) silently default. That's
// the cross-repo bug: `repoPath` degrades to `.` (the cwd), the lenses review the
// WRONG repo and the apply phase commits onto it. Parse a stringified `args`
// defensively (empty string → {}; object used as-is; malformed JSON throws loudly,
// fail-fast). See codex-debate/debate.workflow.js for the same fix and its evidence.
const a = typeof args === 'string' ? (args.trim() ? JSON.parse(args) : {}) : args || {}
const repoPath = a.repoPath || '.'
// The diff base. Resolved to the MERGE-BASE of (rawBase, HEAD) just below, before
// DIFF is built, so the lenses review only what THIS branch changed — not commits
// the base branch gained since the branch forked (those would otherwise appear in
// `git diff base` as the base branch's drift, reviewed as ours). `let` because the
// resolution reassigns it. Idempotent when the caller already passed a merge-base
// SHA (e.g. /be-review).
let base = a.base || 'origin/master'
// Safety backstop only — NOT a deadlock cap. The debate runs until consensus;
// this just keeps a pathologically oscillating debate from running unbounded.
// Hitting it is reported as `unresolved` (needs human), never `deadlock`, and
// should essentially never happen between two good-faith lenses. Raise freely.
const maxRounds = a.maxRounds || 12
// Apply agreed `fix` findings as individual commits (default on). `--no-commit`
// still applies the edits to the working tree, it just leaves them uncommitted.
// No-op when `apply` is false — the apply:false path returns plans in `fixes`
// and never commits; `commit` only gates the in-workflow Apply phase.
const commit = a.commit !== false
// Run the Apply phase at all (default on). `apply: false` skips Phase 3 entirely:
// the debate still settles every finding, but the agreed `fix` plans are RETURNED
// (the `fixes` field) instead of implemented — for callers that want the agreed
// fix plans returned so they can apply them against a tree of their choosing.
const apply = a.apply !== false
// Fold in /code-police as a third, lower-weight voice: it SEEDS findings into
// the debate set but does not get a vote in consensus (only lowy ⇄ hickey do).
const withPolice = a.withPolice === true
// Optional author note on deliberate design decisions, so the lenses don't flag
// intentional choices (e.g. a deliberate fail-open). Threaded into every prompt.
const rationale = (a.rationale || '').trim()
// Model every lens/agent runs on; defaults to MODEL (see top of file). Overridable
// via args to mirror the file's input pattern and to make a model bump a one-liner.
const model = a.model || MODEL
// Mechanical tier (Haiku). The lenses' reviews + the per-finding debate + applying
// an agreed fix all do real reasoning → `model` (Opus, load-bearing for the
// lenses). The merge-base resolver is pure git → run it on `mechModel`.
// Defaults match a direct invocation; /be-review passes it.
const mechModel = a.mechModel || 'haiku'
// Per-worktree scratch for commit-message files; gitignored so it never shows up
// in the diff the lenses review, and parallel debates in different worktrees
// never collide. Only the commit-message files land here.
const workDir = `${repoPath}/.lens-debate`

// Löwy's "electricity" probe — a sharper version of the SAME volatility lens, NOT
// a second voting voice (a separate lens would double-count lowy and reintroduce
// the up-front framing bias this skill avoids). It forces the abstract "where's
// the boundary?" down to the concrete "what plugs into what?", which is exactly
// the abstraction-without-grounding failure mode a lens debate is otherwise prone
// to. Earned its keep on a live run (#1111). Baked into the lowy reviewer's output.
const ELECTRICITY_PROBE = `As a REQUIRED part of your output, apply Löwy's electricity test (Righting Software / The Method) to ground the boundary question in "what plugs into what": name the **receptacle** (the stable interface every consumer plugs into), name the **volatile implementations** that receptacle encapsulates (the interchangeable generators behind it), say whether this is "electricity" (a domain-agnostic utility) or an application concern, and call out where a consumer is forced to "expose the wires" — reach past the receptacle and depend on a specific implementation. If the diff has no such boundary, say so explicitly; do not invent one.`

// The two structural lenses that debate to consensus. code-police, when enabled,
// is appended as a finding SOURCE only — it is not a debater.
const DEBATERS = ['lowy', 'hickey']
const REVIEWERS = [
  { lens: 'lowy', framework: 'volatility-based decomposition — do boundaries encapsulate axes of change? (Lowy / Parnas)', probe: ELECTRICITY_PROBE },
  { lens: 'hickey', framework: 'structural simplicity — independent concerns complected, or one thing fragmented? (Simple Made Easy)' },
]
if (withPolice) REVIEWERS.push({ lens: 'code-police', framework: 'code quality, correctness, and common-mistake review' })

// The result shape's empty collections, shared by the two EARLY returns
// (merge-base-error, clean) so adding a result field is one edit, not a mirror
// edit per return site. The final return carries real values and stays literal.
const EMPTY_RESULT = { settled: [], unresolved: [], applied: [], applyGaps: [], fixes: [], reviews: {}, history: [] }

// Resolve the diff base to the merge-base of (base, HEAD) BEFORE building DIFF
// (which interpolates `base` eagerly), so the lenses review only what this branch
// changed, not the base branch's drift since the fork. A thin mechanical git
// agent (the workflow can't run git itself); grouped under the Review phase.
// Idempotent when `base` is already a merge-base SHA (caller resolved it).
const rawBase = base
const baseRes = await agent(
  `You are a MECHANICAL RUNNER. Run \`git -C ${repoPath} merge-base ${base} HEAD\` and return ONLY the resulting commit SHA (hex) in \`sha\`. If the command FAILS (missing/typoed base, stale ref, unrelated history), return \`sha\`: "" and put the verbatim git error in \`error\` — do NOT fall back to the raw base ref. Do nothing else.`,
  { label: 'resolve:merge-base', phase: 'Review', model: mechModel, schema: { type: 'object', additionalProperties: false, required: ['sha'], properties: { sha: { type: 'string', description: 'the merge-base SHA, or "" on failure' }, error: { type: 'string', description: 'the git error when sha is empty' } } } },
)
// Fail loud on a bad base. Falling back to the raw `${base}` tip would make the
// lenses review the base branch's drift since the fork as if this change made it —
// the exact noise the merge-base removes — so a missing/typoed/stale base aborts.
if (!baseRes?.sha?.trim()) {
  const err = (baseRes?.error || '').trim()
  log(`Aborting: \`git merge-base ${rawBase} HEAD\` failed; the diff scope can't be trusted. Not falling back to the raw ${rawBase} tip.`)
  return {
    ...EMPTY_RESULT,
    status: 'merge-base-error',
    base: rawBase,
    rounds: 0,
    withPolice,
    note: `merge-base of \`${rawBase}\` and HEAD could not be resolved (missing/typoed base, stale ref, or unrelated history), so the review scope is untrustworthy. Fix the base ref (e.g. \`git fetch\`) and re-run.${err ? `\ngit error:\n${err}` : ''}`,
  }
}
base = baseRes.sha.trim()

// How every agent is told to inspect the change. The lenses do NOT trust a
// curated finding list — they read the source themselves (the load-bearing
// lesson from #1109: curation biases the verdict).
const DIFF = `Inspect the FULL change in the repo at \`${repoPath}\` — your shell cwd may be a DIFFERENT worktree, so use \`git -C ${repoPath}\` and ABSOLUTE paths under \`${repoPath}\`: run \`git -C ${repoPath} diff ${base}\` (committed + unstaged) and \`git -C ${repoPath} status --short\` (untracked/new files do NOT appear in the diff), then Read every new/changed file plus enough surrounding code to judge it in context. Ignore the debate's own scratch dir \`.lens-debate/\` if it appears.`

const rationaleBlock = rationale ? `\nAuthor's note on deliberate decisions (do not flag these as defects unless the reasoning is itself wrong):\n${rationale}\n` : ''

// ---------------------------------------------------------------------------
// Schemas — the review and the per-finding debate position
// ---------------------------------------------------------------------------
const FINDINGS_SCHEMA = {
  type: 'object',
  additionalProperties: false,
  required: ['findings'],
  properties: {
    findings: {
      type: 'array',
      description: 'ALL your independent structural findings — every issue worth raising through your lens, no cap. An empty list is fine only for a genuinely clean diff.',
      items: {
        type: 'object',
        additionalProperties: false,
        required: ['title', 'location', 'problem', 'suggestion', 'disposition'],
        properties: {
          title: { type: 'string' },
          location: { type: 'string', description: 'file:line' },
          problem: { type: 'string', description: "the problem in your lens's terms" },
          suggestion: { type: 'string', description: 'a concrete, implementable change' },
          disposition: { type: 'string', enum: ['fix', 'drop'], description: 'fix = worth changing in THIS PR; drop = observation only' },
        },
      },
    },
  },
}

const POSITION_SCHEMA = {
  type: 'object',
  additionalProperties: false,
  required: ['positions'],
  properties: {
    positions: {
      type: 'array',
      description: 'one entry for EVERY contested finding id you were given',
      items: {
        type: 'object',
        additionalProperties: false,
        required: ['id', 'disposition', 'reasoning'],
        properties: {
          id: { type: 'string' },
          disposition: { type: 'string', enum: ['fix', 'drop'] },
          plan: { type: 'string', description: 'if fix: the exact change, implementable' },
          agreesWithPlan: {
            type: 'boolean',
            description:
              "when disposition===fix, true only if you endorse the other lens's plan as-is; if false, your `plan` field is the amendment that must still converge",
          },
          reasoning: { type: 'string', description: 'argue from the code (cite file:line); concede explicitly when the other lens is right' },
        },
      },
    },
  },
}

// One Apply agent implements every agreed fix and commits each in a single
// session, so it returns the full per-fix outcome (not one impl per agent). One
// entry per fix it was handed; `commit` is "" under `--no-commit` or when a fix
// turned out to need no change.
const APPLY_SCHEMA = {
  type: 'object',
  additionalProperties: false,
  required: ['applied'],
  properties: {
    applied: {
      type: 'array',
      description: 'one entry for EVERY agreed fix you were given, in the same order',
      items: {
        type: 'object',
        additionalProperties: false,
        required: ['id', 'summary', 'filesChanged'],
        properties: {
          id: { type: 'string' },
          summary: { type: 'string', description: 'one line: what you changed for this fix' },
          filesChanged: { type: 'array', items: { type: 'string' } },
          commit: { type: 'string', description: 'this fix\'s commit SHA, or "" if nothing was committed' },
        },
      },
    },
  },
}

// ---------------------------------------------------------------------------
// Prompts
// ---------------------------------------------------------------------------
function reviewBrief(lens, framework, probe) {
  const probeBlock = probe ? `\n${probe}\n` : ''
  return `You are the **${lens}** reviewer. First Read \`.claude/skills/${lens}/SKILL.md\` for your framework, then ${DIFF}

Review the change through the **${framework}** lens, INDEPENDENTLY — you are NOT seeing any other reviewer's findings. That independence is the whole point: being handed someone else's curated finding biases the verdict.
${rationaleBlock}${probeBlock}
Give ALL your findings — every structural issue you see through your lens, no cap, at every level (boundary, complecting, naming, duplication, …). Each: a title, a file:line location, the problem in your lens's terms, a concrete suggestion, and a disposition — \`fix\` (worth changing in THIS PR) or \`drop\` (observation only). Don't fabricate issues, but don't hold any back either; an empty list is fine only for a genuinely clean diff.`
}

function findingLine(f) {
  return `### ${f.id} (raised by ${f.origin}) — ${f.title}\n  at ${f.location}; raiser's disposition: ${f.disposition}\n  problem: ${f.problem}\n  suggestion: ${f.suggestion}`
}

function debateBrief(lens, opp, activeFindings, oppPos, settledList, roundNum) {
  const settledNote = settledList.length
    ? `\nALREADY SETTLED (you both agreed — do NOT relitigate, shown for context only):\n${settledList.map((s) => `- ${s.id}: ${s.disposition}`).join('\n')}\n`
    : ''
  const oppBlock = oppPos
    ? `**${opp}'s positions to rebut or concede, point by point:**\n${JSON.stringify(oppPos, null, 2)}\n\nFor each finding you also call \`fix\`, set \`agreesWithPlan\`: true only if you endorse ${opp}'s \`plan\` as-is. If false, your \`plan\` field is the amended plan that must still converge — the finding stays open another round until the plans agree, just like the disposition.`
    : `Round 1 — give your initial disposition on every contested finding below, including ${opp}'s and any from other reviewers.`
  return `You are **${lens}**, cross-examining **${opp}** to reach agreement. First Read \`.claude/skills/${lens}/SKILL.md\` for your framework, then ${DIFF} Ground every call in the source.
${rationaleBlock}
CONTESTED findings — disposition EVERY one (yours, ${opp}'s, and any from other reviewers):
${activeFindings.map(findingLine).join('\n\n')}
${settledNote}
${oppBlock}

Round ${roundNum}. For EVERY contested finding id above, output a disposition (\`fix\` = worth changing in THIS PR / \`drop\` = leave as-is, observation only), a concrete implementable plan if \`fix\`, and reasoning grounded in the code. **The goal is the correct answer for THIS PR, not winning** — concede explicitly ("conceding: …") when ${opp}'s code-grounded argument is right. A \`fix\` is worth it only if it genuinely improves the PR.`
}

// ONE brief for ALL agreed fixes — implemented and committed in a single Apply
// session, so the agent orients on the repo once instead of paying that cost per
// fix (the old form spawned an implement agent AND a commit agent per finding,
// serially). The fixes are independent and their plans already converged in the
// debate, so there's no cross-fix reasoning to isolate; what we keep is one
// commit PER finding so the history still reads finding-by-finding.
function applyAllBrief(fixes, doCommit) {
  const list = fixes
    .map(
      (f) => `### ${f.id} (raised by ${f.origin}) — ${f.title}
  at ${f.location}
  problem: ${f.problem}
  original suggestion (context, not the agreed plan): ${f.suggestion}
  agreed plan: ${f.plan}`,
    )
    .join('\n\n')
  const commitStep = doCommit
    ? `After a fix's edits are done, COMMIT that fix on its own before moving to the next, so each finding maps to one commit and the history reads finding-by-finding. Stage ONLY the files you changed for that fix — never \`git add -A\` or \`git add .\`. Write the message to a file under \`${workDir}\` (run \`mkdir -p ${workDir}\` first) and commit with \`git -C ${repoPath} add -- <files> && git -C ${repoPath} commit -F <msgfile>\`, using EXACTLY this message shape:

  fix(lens): <the fix's title>

  <your one-line summary of the change>

  Agreed by the lowy ⇄ hickey lens debate (finding <id>, raised by <origin>). Not pushed or merged.

Do NOT push. Record each fix's resulting commit SHA (\`git -C ${repoPath} rev-parse HEAD\`) in its \`commit\` field. If a fix turns out to need no change, leave its \`filesChanged\` empty and its \`commit\` "".`
    : `Do NOT git add / commit / push — leave every change in the working tree and set each fix's \`commit\` to "".`
  return `You are implementing the changes that two structural-review lenses (lowy and hickey) independently agreed should be fixed in THIS PR. Work in the repo at \`${repoPath}\` — your shell cwd may be a DIFFERENT worktree, so every file you Read/Edit MUST be an ABSOLUTE path under \`${repoPath}\` and every git command MUST use \`git -C ${repoPath}\`.

Apply each agreed fix below, IN ORDER. The fixes are independent — keep each one tightly scoped to its finding and don't let one bleed into another. Read the surrounding code first so each edit fits the existing style. You may run the project's formatter on files you touched.

${list}

${commitStep}

Return one \`applied\` entry per fix (same order): its id, a one-line summary, the exact files you changed, and the commit SHA (or "").`
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------
const posMap = (res) => Object.fromEntries((res?.positions ?? []).map((p) => [p.id, p]))

// Render the PR comment deterministically from the debate outcome, returned as a
// string so the ORCHESTRATOR posts it verbatim (`gh pr comment -F`) — no agent
// re-improvises a table. Unlike codex-debate there are NO per-round files to
// assemble: the lenses don't read a ledger (feeding them prior reasoning would
// invite entrenchment against conceding), so the comment is the only artifact.
//
// The header chrome (the `## ` title, the badge, the `base.slice(0, 12)`) is
// deliberately kept STRUCTURALLY PARALLEL to codex-debate's ledgerHeader chrome.
// The no-module workflow runtime has no imports, so a truly shared renderer isn't
// available; the two are instead siblings that move together. A house-style change
// (badge emoji, base-slice length, a new metadata row) is a mechanical mirror edit
// — make it here and in codex-debate's ledgerHeader. If the runtime ever admits a
// shared helper file, lift this common chrome there.
// `outcome` is the single mode bit for what happened to the agreed fixes:
// { kind: 'applied', items } when this run implemented them, or
// { kind: 'handed-off', items } when apply:false returned the plans to the
// caller — one param, so "at most one of applied/handed-off" holds by
// construction instead of by convention.
// `applyGaps` (agreed fixes the Apply phase did not cleanly land) is rendered
// HERE, not just in the machine `status`: the SKILL posts this comment verbatim,
// so an apply-incomplete run must surface a warning badge and a dedicated gap
// section instead of advertising `✅ Consensus` and listing the gapped fix as
// `Applied`. Keep this consistent with the status downgrade in Phase 3.
function renderComment({ rounds, settledOut, unresolved, outcome, reviewByLens, withPolice, base, clean, applyGaps = [] }) {
  const gapIds = new Set(applyGaps.map((g) => g.id))
  const badge = applyGaps.length
    ? `⚠️ **Apply incomplete** — ${applyGaps.length} agreed fix(es) not cleanly applied`
    : clean
      ? '✅ **Clean** — every lens found nothing worth raising'
      : unresolved.length === 0
        ? '✅ **Consensus**'
        : `⚠️ **${unresolved.length} unresolved**`
  const counts = Object.entries(reviewByLens)
    .map(([lens, fs]) => `${lens}=${fs.length}`)
    .join(', ')
  // A clean diff never debated, so the "after N round(s)" clause is omitted; the
  // base, the lens roster, and the (all-zero) per-lens counts still ride along so
  // the comment carries the same audit metadata as a debated run.
  const meta = `lowy + hickey${withPolice ? ' + code-police' : ''} · base \`${(base || '').slice(0, 12)}\``
  const lines = [
    '## [⚖️ Lowy ⇄ Hickey lens debate](https://kolu.dev/blog/hickey-lowy/)',
    '',
    clean ? `${badge} · ${meta}` : `${badge} after ${rounds} round(s) · ${meta}`,
    '',
    `Independent findings: ${counts}`,
  ]
  const drops = settledOut.filter((s) => s.agreed && s.disposition === 'drop')
  if (outcome.kind === 'applied') {
    // Only CLEANLY-landed fixes go under `Applied`; a fix in `applyGaps` (missing
    // from the apply output, or changed-but-uncommitted) is NOT applied work and
    // must not be advertised as such under what would otherwise be a consensus
    // badge — it gets its own gap section below.
    const cleanlyApplied = outcome.items.filter((a) => !gapIds.has(a.id))
    if (cleanlyApplied.length) {
      lines.push('', `### Applied (${cleanlyApplied.length})`)
      cleanlyApplied.forEach((a) => lines.push(`- \`${a.id}\` ${a.title}${a.commit ? ` — commit \`${a.commit.slice(0, 9)}\`` : ' — (uncommitted)'}`))
    }
    if (applyGaps.length) {
      lines.push('', `### Apply incomplete — needs reconcile (${applyGaps.length})`)
      const reasonText = { 'missing-from-output': 'not confirmed applied (absent from apply output)', uncommitted: 'changed but not committed (per-fix commit missing)' }
      applyGaps.forEach((g) => {
        const item = outcome.items.find((a) => a.id === g.id)
        const title = item?.title ? ` ${item.title}` : ''
        lines.push(`- \`${g.id}\`${title} — ${reasonText[g.reason] ?? g.reason}`)
      })
    }
  }
  // apply:false runs hand the agreed plans to the caller instead of implementing
  // them; the comment records the handoff so the trail still shows what was agreed
  // (the caller appends its own apply outcomes when it posts this).
  if (outcome.kind === 'handed-off' && outcome.items.length) {
    lines.push('', `### Agreed fixes — handed off to the caller (${outcome.items.length})`)
    outcome.items.forEach((f) => lines.push(`- \`${f.id}\` ${f.title} (${f.location})`))
  }
  if (drops.length) {
    lines.push('', `### Agreed — no change (${drops.length})`)
    drops.forEach((d) => lines.push(`- \`${d.id}\` ${d.title} (${d.location})`))
  }
  if (unresolved.length) {
    lines.push('', `### Unresolved — needs human (${unresolved.length})`)
    // Surface BOTH lenses' full final positions (disposition + reasoning + any
    // plan), not just the bare verdict — a human adjudicating needs the actual
    // disagreement, which lives in each side's reasoning/plan text.
    unresolved.forEach((u) => {
      lines.push('', `- \`${u.id}\` ${u.title} (${u.location})`)
      for (const lens of ['lowy', 'hickey']) {
        const p = u[lens]
        const verdict = p?.disposition ?? '?'
        const reasoning = p?.reasoning ? ` — ${p.reasoning}` : ''
        lines.push(`  - **${lens}**: ${verdict}${reasoning}`)
        if (p?.plan?.trim()) lines.push(`    - plan: ${p.plan}`)
      }
    })
  }
  return lines.join('\n')
}

// ---------------------------------------------------------------------------
// Phase 1 — independent parallel review
// ---------------------------------------------------------------------------
phase('Review')

const reviews = await parallel(
  REVIEWERS.map((r) => () =>
    agent(reviewBrief(r.lens, r.framework, r.probe), { label: `review:${r.lens}`, phase: 'Review', model, schema: FINDINGS_SCHEMA }),
  ),
)

const reviewByLens = {}
const combined = []
REVIEWERS.forEach((r, idx) => {
  const findings = reviews[idx]?.findings ?? []
  reviewByLens[r.lens] = findings
  findings.forEach((f, i) => combined.push({ id: `${r.lens}-${i + 1}`, origin: r.lens, ...f }))
})
log(`Independent findings: ${REVIEWERS.map((r) => `${r.lens}=${reviewByLens[r.lens].length}`).join(', ')}`)

if (combined.length === 0) {
  // Route the clean outcome through the SAME renderer as a debated run so the
  // comment carries the same audit metadata (base, lens roster, per-lens counts,
  // whether code-police ran) instead of a bare one-liner.
  const comment = renderComment({ rounds: 0, settledOut: [], unresolved: [], outcome: { kind: apply ? 'applied' : 'handed-off', items: [] }, reviewByLens, withPolice, base, clean: true })
  return { ...EMPTY_RESULT, status: 'clean', rounds: 0, base, withPolice, note: 'every lens found nothing worth raising', reviews: reviewByLens, comment }
}

// ---------------------------------------------------------------------------
// Phase 2 — debate to consensus. NO deadlock exit: the loop runs until every
// finding is agreed. Agreed findings LOCK (leave the active set), so the
// contested set is monotonically non-increasing — the debate can only shrink.
// Sequential reveal (lowy posts, hickey answers lowy's CURRENT positions) lets
// the two land together rather than chase each other's stale positions.
// ---------------------------------------------------------------------------
phase('Debate')

const settled = {} // id -> { disposition, plan, lowy, hickey }
let activeIds = combined.map((f) => f.id)
let lowyPrev = null
let hickeyPrev = null
const history = []
let status = 'unresolved'
let rounds = 0

for (let r = 1; r <= maxRounds && activeIds.length > 0; r++) {
  rounds = r
  const activeFindings = combined.filter((f) => activeIds.includes(f.id))
  const settledList = Object.entries(settled).map(([id, s]) => ({ id, disposition: s.disposition }))

  const lowyRes = await agent(debateBrief('lowy', 'hickey', activeFindings, hickeyPrev, settledList, r), {
    label: `lowy:round${r}`,
    phase: 'Debate',
    model,
    schema: POSITION_SCHEMA,
  })
  const lowyPos = posMap(lowyRes)

  const hickeyRes = await agent(debateBrief('hickey', 'lowy', activeFindings, lowyPos, settledList, r), {
    label: `hickey:round${r}`,
    phase: 'Debate',
    model,
    schema: POSITION_SCHEMA,
  })
  const hickeyPos = posMap(hickeyRes)
  lowyPrev = lowyPos
  hickeyPrev = hickeyPos

  const per = []
  for (const id of [...activeIds]) {
    const l = lowyPos[id]
    const h = hickeyPos[id]
    // For a `fix`, agreement requires the second poster (hickey, who has seen
    // lowy's positions) to endorse lowy's plan as-is — otherwise the finding
    // stays active so the plan converges the same way the disposition does.
    // `plan` is optional in the schema, so a `fix` can only settle once lowy has
    // actually supplied a non-empty plan: endorsing an absent plan is not
    // consensus, and Apply must never run on a `plan: undefined` (it would fall
    // back to a vague placeholder and commit an arbitrary edit as "agreed").
    const lowyHasPlan = !!(l && typeof l.plan === 'string' && l.plan.trim())
    const agreed = !!(
      l &&
      h &&
      l.disposition === h.disposition &&
      (l.disposition !== 'fix' || (h.agreesWithPlan === true && lowyHasPlan))
    )
    per.push({ id, lowy: l?.disposition ?? '?', hickey: h?.disposition ?? '?', agreed })
    if (agreed) {
      // Endorsement guarantees l.plan is the converged text; no arbitrary fallback.
      settled[id] = { disposition: l.disposition, plan: l.disposition === 'fix' ? l.plan : undefined, lowy: l, hickey: h }
      activeIds = activeIds.filter((x) => x !== id)
    }
  }
  history.push({ round: r, per })
  log(`Round ${r}: ${per.map((p) => `${p.id} ${p.lowy}/${p.hickey}${p.agreed ? '✓' : '✗'}`).join('  ')} | settled ${Object.keys(settled).length}/${combined.length}`)

  if (activeIds.length === 0) {
    status = 'consensus'
    break
  }
}

// Final per-finding verdict: agreed ones carry the consensus disposition;
// any still-contested ones are surfaced (unresolved → human), never silently dropped.
const settledOut = combined.map((f) => {
  const s = settled[f.id]
  if (s) {
    return { id: f.id, origin: f.origin, title: f.title, location: f.location, problem: f.problem, suggestion: f.suggestion, agreed: true, disposition: s.disposition, plan: s.plan, lowy: s.lowy, hickey: s.hickey }
  }
  return { id: f.id, origin: f.origin, title: f.title, location: f.location, problem: f.problem, suggestion: f.suggestion, agreed: false, disposition: 'unresolved', plan: undefined, lowy: lowyPrev?.[f.id], hickey: hickeyPrev?.[f.id] }
})
const unresolved = settledOut.filter((s) => !s.agreed)
log(`Debate ended: ${status} after ${rounds} round(s); ${settledOut.length - unresolved.length}/${settledOut.length} settled, ${unresolved.length} unresolved.`)

// ---------------------------------------------------------------------------
// Phase 3 — apply every agreed `fix` finding in a SINGLE session, one commit
// per finding. One agent orients on the repo once and applies all the fixes,
// rather than paying a fresh implement+commit agent (and its re-orientation
// cost) per finding; the fixes are independent and their plans already
// converged, so there's no cross-fix reasoning to isolate. Skipped wholesale
// under `apply: false`: the agreed plans are returned in `fixes` for the caller
// to implement against whatever tree it chooses.
// ---------------------------------------------------------------------------
const fixes = settledOut.filter((s) => s.agreed && s.disposition === 'fix')
let applied = []
// Agreed fixes the Apply phase did not cleanly land. Two failure shapes, both of
// which would otherwise be rendered as "applied" and reported under a consensus:
//  - missing: the agent dropped the fix from its output entirely (no entry, no
//    files) — we can't tell if it was applied, so it must not be reported as done.
//  - uncommitted: in commit mode the agent changed files for the fix but returned
//    no SHA — its per-fix commit didn't land, breaking "one commit per fix".
// The edits (when present) stay in the tree, so this is a status downgrade, not a
// hard abort: the caller reconciles the gap rather than losing a converged debate.
const applyGaps = []
if (apply && fixes.length) {
  phase('Apply')
  const res = await agent(applyAllBrief(fixes, commit), { label: 'apply:all', phase: 'Apply', model, schema: APPLY_SCHEMA })
  const byId = Object.fromEntries((res?.applied ?? []).map((a) => [a.id, a]))
  // Re-key off the agreed `fixes` (not the agent's array) so a fix the agent
  // dropped from its output still surfaces — as 0 files / uncommitted — instead
  // of vanishing from `applied` and the PR comment.
  applied = fixes.map((f) => {
    const entry = byId[f.id]
    const a = entry || {}
    const sha = (a.commit || '').trim()
    const files = a.filesChanged ?? []
    if (!entry) {
      // The agent never reported this agreed fix. We can't confirm it was applied,
      // so flag it rather than render a phantom 0-file "applied" row as success.
      applyGaps.push({ id: f.id, reason: 'missing-from-output' })
      log(`Apply ${f.id}: agreed fix absent from apply-agent output — not confirmed applied`)
    } else if (commit && !sha && files.length > 0) {
      // Reported changed-but-uncommitted in commit mode: the per-fix commit the
      // agent was told to make didn't land. Surface it as a gap, not a clean apply.
      applyGaps.push({ id: f.id, reason: 'uncommitted' })
      log(`Apply ${f.id}: agent changed ${files.length} file(s) but returned no commit SHA`)
    }
    return { id: f.id, title: f.title, files, commit: sha || null }
  })
  applied.forEach((a) => log(`Applied ${a.id}: ${a.files.length} file(s)${a.commit ? `, committed ${a.commit.slice(0, 9)}` : ' (uncommitted)'}`))
  // A converged debate whose fixes didn't cleanly land is NOT a clean consensus:
  // downgrade so /be-review (which keys off this status) and the comment don't
  // advertise success over an unconfirmed/uncommitted fix. Only touch a status
  // that was otherwise clean ('consensus'/'clean'); 'unresolved' already signals
  // the human must act.
  if (applyGaps.length && (status === 'consensus' || status === 'clean')) {
    const prior = status
    status = 'apply-incomplete'
    log(`Apply incomplete: ${applyGaps.map((g) => `${g.id} (${g.reason})`).join(', ')} — downgrading ${prior} to apply-incomplete.`)
  }
} else if (fixes.length) {
  log(`Apply skipped (apply: false) — returning ${fixes.length} agreed fix plan(s) to the caller.`)
}

return {
  status,
  rounds,
  base,
  withPolice,
  settled: settledOut,
  unresolved,
  applied,
  // Agreed fixes that didn't cleanly land (missing from the apply output, or
  // changed-but-uncommitted). Empty unless status is 'apply-incomplete'; lets the
  // caller pinpoint which fix to reconcile.
  applyGaps,
  // The agreed `fix` findings with their converged plans — the caller's
  // change-request payload under `apply: false` (redundant with `settled` when
  // the Apply phase ran, but always present so consumers need not re-filter).
  fixes,
  reviews: reviewByLens,
  history,
  comment: renderComment({ rounds, settledOut, unresolved, outcome: apply ? { kind: 'applied', items: applied } : { kind: 'handed-off', items: fixes }, reviewByLens, withPolice, base, applyGaps }),
}
