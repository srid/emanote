export const meta = {
  name: 'codex-debate',
  description: 'Run a codex<->claude review debate on the current diff until they reach consensus (no round cap, no deadlock exit)',
  phases: [
    { title: 'Debate', detail: 'codex reviews -> claude responds, round after round' },
  ],
}

// ---------------------------------------------------------------------------
// Inputs (passed via the Workflow tool's `args`)
// ---------------------------------------------------------------------------
// The harness JSON-ENCODES `args` before the workflow sees it, so `args` arrives as
// a STRING even when the caller passed a real object — a bare `args.repoPath` is then
// `undefined` and EVERY input below silently falls back to its default. That's the
// cross-repo bug: `repoPath` degrades to `.` (the cwd), the debate runs `git -C .`
// against the WRONG repo, and it reports a vacuous "clean" (or, worse, commits fixes
// onto the cwd repo). It also means `base`/`model`/`rationale`/`context` never thread
// through; same-repo runs only "work" by cwd coincidence. So parse a stringified
// `args` defensively here: an empty string means "no args" → {}; an already-parsed
// object is used as-is; malformed JSON THROWS loudly (fail-fast) rather than degrading
// to a silent default.
const a = typeof args === 'string' ? (args.trim() ? JSON.parse(args) : {}) : args || {}
const repoPath = a.repoPath || '.'
// The diff base. Resolved to the MERGE-BASE of (rawBase, HEAD) just before the
// debate (see phase 'Debate') so commits rawBase gained since the branch forked
// aren't reviewed as if this change made them. `let` because that resolution
// reassigns it; every prompt reads the resolved value. (Idempotent when the
// caller already passed a merge-base SHA, e.g. /be-review.)
let base = a.base || 'origin/master'
// Where the generated skill lives, so the codex runner can find codex-review.sh.
const skillDir = a.skillDir || '.claude/skills/codex-debate'
// Per-worktree scratch dir for rebuttal/verdict files. Derived from repoPath
// (the worktree root === $PWD) so parallel debates in DIFFERENT worktrees never
// collide on shared /tmp paths, and `.codex-debate/` is gitignored so these
// files never pollute the diff codex reviews.
const workDir = `${repoPath}/.codex-debate`
// POSIX single-quote a path for safe interpolation into a shell command. Wraps
// in single quotes (so spaces, globs, and shell metacharacters are inert) and
// escapes any embedded single quote via the '\'' idiom. Used for the one
// DESTRUCTIVE command (the ledger `rm -f` below); the benign `mkdir -p` prompts
// elsewhere can tolerate an unquoted path, but a mistargeted `rm -f` cannot.
const shq = (s) => `'${String(s).replace(/'/g, `'\\''`)}'`
// The debate is recorded as small Markdown SECTION FILES under the gitignored
// scratch dir — TWO per round: `section-NNN-1-codex.md` (codex's verdict) and
// `section-NNN-2-claude.md` (the author's dispositions). Each is written by the
// party that OWNS its content, never forced through a structured payload:
//   * codex's section — a Haiku writer renders the small STRUCTURED verdict
//     (approval + findings) to disk; faithful, nothing to bloat.
//   * claude's section — the AUTHOR writes its OWN per-finding dispositions
//     directly (it's already editing the tree), exactly the way codex writes its
//     verdict to a path. This is the STRUCTURAL fix for the old crash: the author
//     used to pour its whole narrative into a structured field, which overflowed
//     the StructuredOutput encoding and silently dropped the required array until
//     the retry cap tripped. Now the narrative goes to the file and the author's
//     structured return is a MINIMAL ACK (filesChanged/commitSha/done) whose size
//     is decoupled from the finding count entirely, so it can never overflow.
// These section files serve THREE roles at once, no copy ever re-typed by a weak
// agent: (1) the author's cross-round memory (it cats them for full history);
// (2) the REBUTTAL codex reads next round — codex-review.sh `cat`s the author's
// section file straight into its prompt, so codex still sees every disposition;
// (3) the published PR comment, which the orchestrator assembles by `cat`-ing the
// section files after a small in-process header (see ledgerHeader / commentHeader)
// — a deterministic shell concat, never re-rendered through an agent. codex is NOT
// a memory reader: it keeps its own warm session, so it only ever reads the one
// rebuttal file, not the whole ledger.
// Commit each round's changes individually (default on). The author commits its
// OWN round in-session — it already edits the tree, so it stages exactly what it
// changed and writes a message carrying the debate context (codex's findings +
// its dispositions). Never pushes or merges — that stays the human's call.
const commit = a.commit !== false
// Model tiers. The claude-author round does real reasoning (fixing/disputing
// codex's findings, and committing its own round) → `model` (Opus). Everything
// else here is mechanical — the codex runner just shells out to codex-review.sh
// and copies the verdict, the codex-section writer dumps a rendered verdict to a
// file, the merge-base resolver runs one git command → `mechModel`
// (Haiku). (The CLAUDE section is written by the author itself, on `model`, as part
// of its round — not by a mechanical writer.) Defaults match a direct invocation;
// /be-review passes both explicitly.
const model = a.model || 'opus'
const mechModel = a.mechModel || 'haiku'
// Fidelity tier (Sonnet). One "mechanical" job isn't a trivial command but a
// faithful COPY: the codex runner reads codex's verdict JSON off disk and must
// return it byte-for-byte. A paraphrase silently corrupts the debate (and schema
// validation checks the verdict's SHAPE, not its wording), and Haiku is the
// weakest tier for verbatim reproduction — so the verdict relay runs a notch up.
// Still far cheaper/faster than Opus; the real reviewing is codex's, not this
// agent's. The small per-round codex-section writes stay on Haiku (tiny payloads).
const copyModel = a.copyModel || 'sonnet'

// --- Context the Claude implementor INHERITS --------------------------------
// Two optional notes the CALLER threads in so the implementor (the Claude author)
// no longer reasons from the diff alone — the gap that made it re-derive the
// change's intent every round and re-litigate deliberate choices codex (rightly,
// on a bare diff) flags.
//
// `context` (#1): the MAIN-AGENT context — what this change is FOR (the task/intent
// and key decisions the orchestrator already holds). Injected into the implementor
// EVERY round: agent() is one-shot and Claude isn't headless under Max auth, so it
// can't be resumed the way codex is — re-injection is how it "inherits" at all.
// Deliberately NOT given to codex, which stays an independent reviewer of the
// actual code rather than the author's narrative.
const context = (a.context || '').trim()
// `rationale` (#2): the author's note on DELIBERATE decisions — the same note
// /lens-debate already accepts, now threaded here too. Given to BOTH sides: codex
// (its round-1 prompt, via codexReviews → codex-review.sh — so the reviewer doesn't
// raise them at the source; codex's warm session carries the note across rounds)
// AND the implementor (so it DISPUTES, rather than "fixes", a finding that
// contradicts a deliberate choice).
const rationale = (a.rationale || '').trim()
// The two notes as ready-to-interpolate implementor-prompt blocks. Empty when the
// note is absent, so the prompt stays byte-identical to the contextless form then.
const contextBlock = context
  ? `\nContext you INHERIT from the main agent — what this change is FOR (its task/intent and key decisions). Weigh codex's findings against it: a finding that contradicts this intent is a candidate to DISPUTE, not blindly fix.\n${context}\n`
  : ''
const rationaleBlock = rationale
  ? `\nAuthor's note on DELIBERATE decisions (chosen on purpose — do NOT "fix" them away; dispute the finding unless codex shows the decision itself is wrong):\n${rationale}\n`
  : ''
// codex reads the rationale from a file (it's constant across rounds, written once
// before the loop); `-` means "no rationale" to codex-review.sh.
const rationaleFile = `${workDir}/rationale.md`
const rationaleFileArg = rationale ? rationaleFile : '-'

// The reasoning effort codex runs at, scoped to the debate. This JS constant is
// the SINGLE home for the value: it is passed script-ward (a 4th positional arg
// to codex-review.sh, which sets `-c model_reasoning_effort`) and read by
// ledgerHeader for the published comment, so the `-c` flag and the header both
// derive from here via the one-directional invocation channel — no literal
// repeated across files held together by "remember to update all of them".
const REASONING_EFFORT = 'xhigh'

// ---------------------------------------------------------------------------
// Schemas — the codex verdict schema mirrors scripts/codex-verdict.schema.json
// so the runner agent returns the same shape codex was constrained to.
// ---------------------------------------------------------------------------
const FINDING = {
  type: 'object',
  additionalProperties: false,
  properties: {
    id: { type: 'string' },
    severity: { type: 'string', enum: ['blocking', 'major', 'minor', 'nit'] },
    location: { type: 'string' },
    issue: { type: 'string' },
    suggestion: { type: 'string' },
    status: { type: 'string', enum: ['open', 'resolved'] },
  },
  required: ['id', 'severity', 'location', 'issue', 'suggestion', 'status'],
}

const CODEX_VERDICT_SCHEMA = {
  type: 'object',
  additionalProperties: false,
  properties: {
    approved: { type: 'boolean' },
    summary: { type: 'string' },
    findings: { type: 'array', items: FINDING },
    responseToRebuttal: { type: 'string' },
    // Set by scripts/codex-review.sh ONLY when codex itself failed to produce a
    // verdict (broken/unavailable reviewer). It is the machine-detectable fatal
    // signal the loop aborts on — infrastructure failure, not a debate outcome.
    reviewerError: { type: 'boolean' },
  },
  required: ['approved', 'summary', 'findings', 'responseToRebuttal'],
}

// The author's structured return is a MINIMAL ACK by design — no `summary`, no
// per-finding `actions`. The full narrative (the per-finding dispositions AND the
// round summary) is written by the author to its section file instead (see
// claudeResponds), exactly the way codex writes its verdict to a path. This is the
// structural fix for the old crash: a large structured payload (the author pouring
// its whole F1/F2/F3 narrative into `summary` + one `detail` per finding)
// overflowed the StructuredOutput encoding, which silently dropped the required
// array and tripped the retry cap. With only these few small, fixed fields the
// payload size is decoupled from the finding count entirely, so it can never
// overflow. `filesChanged` is bounded by the files touched (not narrative);
// `commitSha` is one hash; `done` is a flag.
const CLAUDE_RESPONSE_SCHEMA = {
  type: 'object',
  additionalProperties: false,
  properties: {
    filesChanged: { type: 'array', items: { type: 'string' } },
    // The author commits its own round (it already edits the tree), so it returns
    // the resulting SHA here. "" when it changed nothing or ran under --no-commit.
    commitSha: { type: 'string' },
    done: { type: 'boolean' },
  },
  required: ['filesChanged', 'done'],
}

// Consensus = no finding left open, any severity. The loop runs until codex
// resolves every one (CLAUDE fixed it, or codex conceded a dispute). No cap.
function openFindings(verdict) {
  return (verdict.findings || []).filter((f) => f.status !== 'resolved')
}

// ---------------------------------------------------------------------------
// The two debaters
// ---------------------------------------------------------------------------
async function codexReviews(round, rebuttalPath) {
  const verdictPath = `${workDir}/verdict-${round}.json`
  // The rebuttal codex reads is the author's PRIOR-round disposition section file
  // (it wrote it itself; codex-review.sh `cat`s it straight into codex's prompt).
  // No inline blob, no separate rebuttal-file write step — the file already exists.
  // `-` on round 1 (no prior author turn yet).
  const rebuttalArg = rebuttalPath || '-'
  const rebuttalNote = rebuttalPath
    ? `   (\`${rebuttalPath}\` is the author's prior-round disposition section — codex reads it as the rebuttal. If it's somehow missing, the script proceeds with no rebuttal and warns; that's fine.)`
    : `   (No prior rebuttal this round — the \`-\` argument tells the script there's none.)`

  const prompt = `You are a MECHANICAL RUNNER for one round of an automated code-review debate. Do exactly the steps below and nothing else. Do NOT review the code yourself, do NOT edit any repository files, do NOT add commentary.

First ensure the scratch dir exists: \`mkdir -p ${workDir}\`.

1. Run (cd into the repo root so the script's internal \`git diff\`/\`git status\` target THIS worktree — your shell cwd may be a different worktree):
   \`cd ${repoPath} && bash ${skillDir}/scripts/codex-review.sh ${base} ${rebuttalArg} ${verdictPath} ${REASONING_EFFORT} ${rationaleFileArg}\`
${rebuttalNote}

   This shells out to the codex CLI as a read-only reviewer; it can take 1-3 minutes. It prints a JSON verdict as its final stdout and also writes it to the \`-o\` path.

2. Read \`${verdictPath}\` and return its exact contents as your structured output. Copy the values faithfully; do not paraphrase or "improve" them.`

  return agent(prompt, {
    label: `codex:round${round}`,
    phase: 'Debate',
    model: copyModel, // not trivial: must relay codex's verdict JSON faithfully
    schema: CODEX_VERDICT_SCHEMA,
  })
}

async function claudeResponds(round, verdict, doCommit) {
  // WARM AUTHOR. We can't truly resume the Claude author (agent() is one-shot,
  // and Claude isn't headless under Max auth, so there's no session to resume the
  // way `codex exec resume` carries codex's reasoning forward). The achievable
  // equivalent is context, not state: every follow-up round the author reads the
  // per-round section files — the record of every prior round's findings (codex's
  // section, Haiku-written) and its OWN dispositions (the claude section it wrote
  // itself last round) — and builds on them instead of re-deriving the diff. codex's
  // section is written each round in the loop and the author writes its claude
  // section as the LAST step of its own turn, so on round N>1 the files already hold
  // rounds 1..N-1. Round 1 has none yet, so its prompt is byte-identical to a cold start.
  const priorBlock =
    round > 1
      ? `This is a FOLLOW-UP round. Every prior round is recorded as a small Markdown file under the debate's scratch dir — read them FIRST for the full history (codex's past findings and YOUR own dispositions):
  \`cat ${workDir}/section-*.md\`   (or Read them individually; if none exist, fall back to the diff + the verdict below)
Build on what you already did; don't re-derive the diff from scratch, and don't re-fix or re-litigate anything already settled. For any finding you DISPUTED, check codex's \`responseToRebuttal\` in the verdict below: if codex conceded, you're done with it; if codex held firm, weigh its reasoning and either fix it or hold with a sharper argument. Spend this round on findings still \`open\` plus any new ones.

`
      : ''
  const prompt = `You authored the changes on this branch. CODEX reviewed them and returned the verdict below — what do you think? Fix what you agree with, push back (with reasons) on what you don't.

Work in the repo at \`${repoPath}\` — your shell cwd may be a different worktree, so use ABSOLUTE paths under it and \`git -C ${repoPath}\`. See the change with \`git -C ${repoPath} diff ${base}\`.
${contextBlock}${rationaleBlock}
${priorBlock}CODEX's verdict (JSON):
${JSON.stringify(verdict, null, 2)}

Address EVERY finding, any severity (don't skip minors/nits):
  - agree → fix it in the working tree; disposition "fixed".
  - disagree → leave the code, dispute it with a specific technical reason (cite file:line); disposition "disputed". Concede when codex is right.
  - partly → fix the valid part, explain the rest; disposition "partial".
  - NOT a code edit for this worktree — a downstream / ship-phase / process gate (a companion repo pinning this repo's FINAL post-review HEAD, a CI/release step, a cross-repo PR) that cannot be satisfied mid-review → disposition "disputed", and SAY EXPLICITLY it is a ship-phase gate, not a code change, so codex marks it resolved-and-deferred rather than holding the debate open on something neither side can land here. Use this ONLY for a genuine non-code/process gate, never to dodge a code change you'd rather not make.

You may run the formatter on files you touched. SELF-VERIFY before you claim "fixed": a fix you didn't run isn't a fix. Run the project's own fast static-check gate (its lint + typecheck task — e.g. \`just check\`/\`npm run lint\`; discover it from the repo, don't hand-roll one) over your edits and make it pass; only mark a finding "fixed" once the gate is green. Never claim a lint won't fire without running it — that's the exact "I watched it work" gap that lands a red tree on the next step. If the gate stays red after a genuine attempt, say so in the disposition rather than reporting a clean "fixed". ${
    doCommit
      ? `Once you've addressed every finding AND you actually changed files (and the static-check gate is green), COMMIT this round's work yourself — the debate records one commit per round. Stage ONLY the files you changed (never \`git add -A\` or \`git add .\`) and commit with \`git -C ${repoPath}\`, subject \`fix: codex review — debate round ${round}\` and a body that summarizes your changes plus, briefly, codex's findings and how you dispositioned each. Do NOT push. You'll return the resulting SHA (\`git -C ${repoPath} rev-parse HEAD\`) in \`commitSha\`.`
      : `Edit the working tree only — do NOT git add/commit/push; you'll leave \`commitSha\` empty.`
  }

RECORD your dispositions to a file — this is the durable trail and the heart of how this debate stays robust. The next round's author reads it as memory, codex reads it as your rebuttal, and it's what gets posted to the PR. So the FULL per-finding narrative lives in this file, NEVER in your structured return. Using the Write tool, create the file \`${claudeSectionFile(round)}\` (overwrite any existing one) with EXACTLY this Markdown shape:

**claude** — <one sentence: what you did this round>

- \`F1\` **fixed** — <why; what you changed; cite file:line>
- \`F2\` **disputed** — <the specific technical reason codex is wrong; cite file:line>
- \`F3\` **partial** — <what you fixed and what you didn't, and why>
${doCommit ? '... one bullet PER finding (disposition ∈ fixed | disputed | partial), then:\n\ncommit: `<the SHA you committed, or omit this whole line if you changed nothing>`' : '... one bullet PER finding (disposition ∈ fixed | disputed | partial). (No commit line — running under --no-commit.)'}

CRITICAL — your STRUCTURED RETURN IS A MINIMAL ACK, nothing more. Return ONLY: \`filesChanged\` (the source paths you edited — never the scratch file above), \`commitSha\` (the SHA, or "" if you changed nothing / under --no-commit), and \`done\` (true once you've addressed every finding this round). Do NOT put your summary or any per-finding detail in the structured output — all of that goes in the section file and the commit body. (This is deliberate: past runs CRASHED because the author poured a multi-finding narrative into a structured field, which overflowed the output encoding and dropped a required field until the retry cap tripped. Writing the narrative to the file instead removes that failure by construction — keep the structured payload tiny.)`

  return agent(prompt, {
    label: `claude:round${round}`,
    phase: 'Debate',
    model, // deep reasoning: the author fixing/disputing real findings
    schema: CLAUDE_RESPONSE_SCHEMA,
  })
}

// ---------------------------------------------------------------------------
// The shared ledger — section files on disk, assembled into the comment by the
// orchestrator (a faithful `cat`). The workflow renders only the small CODEX
// section (from the structured verdict) and the comment HEADER in-process; the
// author writes its own claude section (see claudeResponds).
// ---------------------------------------------------------------------------
// One codex finding as a Markdown bullet — the single projection of a finding's
// fields for the ledger section. (The per-round commit message is now written by
// the author itself, in its own session, so this no longer feeds it.)
function findingBullet(f) {
  return `- \`${f.id}\` · ${f.severity} · ${f.status} — ${f.issue} (${f.location})`
}

// One round's findings, as a Markdown list. Shared by the codex-section renderer.
function renderFindings(verdict) {
  const list = (verdict.findings || []).map((f) => findingBullet(f)).join('\n')
  return list || '- _(none)_'
}

// CODEX's side of one round, as a Markdown section: its verdict, findings, and
// response to the author's rebuttal. Rendered in-process from the STRUCTURED
// verdict (small, faithful) and written to disk by a Haiku writer. The author's
// side is a SEPARATE file the author writes itself (its dispositions), so this
// renderer no longer touches the claude response at all — that's the decoupling
// that keeps the author's structured payload minimal. This carries the round's
// `### Round N` header (it's written every round, including the terminal one).
function codexSection(round, verdict) {
  const lines = [
    `### Round ${round}`,
    '',
    `**codex** — approved: \`${verdict.approved}\``,
    '',
    verdict.summary,
    '',
    'Findings:',
    renderFindings(verdict),
  ]
  if (verdict.responseToRebuttal) lines.push('', `_codex on the rebuttal:_ ${verdict.responseToRebuttal}`)
  return lines.join('\n')
}

// The comment header (small). The full comment is this header followed by the
// per-round section files, `cat`-ed together by the orchestrator (see SKILL step 3)
// — a deterministic shell concat, never re-rendered through an agent. The workflow
// returns this header as `commentHeader`; it can't read the section files itself
// (no I/O), so it hands the orchestrator the header + the section dir.
//
// This header's chrome (the `## ` title, the badge, the `base.slice(0, 12)`) is
// deliberately kept STRUCTURALLY PARALLEL to lens-debate's renderComment header
// chrome. The no-module workflow runtime has no imports, so a truly shared
// renderer isn't available; the two are instead siblings that move together. A
// house-style change (badge emoji, base-slice length, a new metadata row) is a
// mechanical mirror edit — make it here and in lens-debate's renderComment. If
// the runtime ever admits a shared helper file, lift this common chrome there.
function ledgerHeader(meta) {
  const badge = meta.status === 'consensus' ? '✅ **Consensus**' : `⚠️ **${meta.status}**`
  return `## Codex ⇄ Claude debate\n\n${badge} after ${meta.rounds} round(s) · codex reviewed at \`${meta.reasoningEffort}\` reasoning effort · base \`${(meta.base || '').slice(0, 12)}\``
}

// Per-round section file paths. TWO files per round, named so the `section-*.md`
// glob sorts both into round order AND codex-before-claude WITHIN a round
// (`section-001-1-codex.md` < `section-001-2-claude.md` < `section-002-1-codex.md`),
// so `cat`-ing the glob yields the debate in chronological order for both the
// author's memory read and the comment assembly. Zero-padded for the same reason.
const codexSectionFile = (round) => `${workDir}/section-${String(round).padStart(3, '0')}-1-codex.md`
const claudeSectionFile = (round) => `${workDir}/section-${String(round).padStart(3, '0')}-2-claude.md`

// Drop a string to a scratch file via a mechanical Haiku writer — the single home
// for the "write this content to this path" idiom (the workflow can't do file I/O
// itself, and Claude isn't headless, so a tiny agent does it). Both the per-round
// codex-section writer and the one-shot rationale writer route through here;
// payloads are small (one round / one note) so Haiku is safe, and overwriting is
// idempotent (safe on a resume).
function writeFileAgent(path, content, label) {
  const prompt = `You are a MECHANICAL WRITER. Do exactly these steps and nothing else — do not edit any other file, do not run git, do not add commentary.

1. Ensure the scratch dir exists: \`mkdir -p ${workDir}\`.
2. Using the Write tool, create \`${path}\` with EXACTLY this content, overwriting any existing file:

${content}`
  return agent(prompt, { label, phase: 'Debate', model: mechModel })
}

// Write ONE round's CODEX section to its own small file (the claude section is the
// author's own write, in claudeResponds). The author reads these as cross-round
// memory and the orchestrator cats them into the posted comment. No whole-ledger
// retype: the payload is just this round's structured verdict, rendered.
async function writeCodexSection(round, verdict) {
  return writeFileAgent(codexSectionFile(round), codexSection(round, verdict), `ledger:codex:round${round}`)
}

// VERIFY the author actually wrote its disposition section this round. The author's
// claude section is the load-bearing handoff: it's the next round's rebuttal (codex
// reads it), the author's own cross-round memory, AND part of the posted comment.
// If the author skipped the Write, `lastClaudeSectionPath` would still point at a
// path that doesn't exist — codex-review.sh would warn and proceed with an EMPTY
// rebuttal, and the debate could still converge over a hole in the trail. So after
// every author turn we deterministically check the file exists, is non-empty, AND
// carries a backticked disposition marker for EVERY open finding this round. The
// workflow has no file I/O of its own, so a thin mechanical agent runs `test -s`
// plus an exact `grep -F` per finding id. We do NOT parse prose or score the
// disposition text — only that a `\`Fn\`` token is present, which is an exact,
// bounded check the author prompt already mandates ("one bullet PER finding",
// each id backticked). This closes the hole the file-as-source-of-truth opened:
// a non-empty but INCOMPLETE section (omitting `F2`) used to advance the rebuttal
// pointer and could converge over a per-finding hole in the durable trail. A miss
// — empty file OR any missing finding id — is recorded as a section gap and
// downgrades the terminal status (see below), the same fail-loud-not-silent
// treatment as a missed commit. Codex's own next-round re-review still polices
// the SUBSTANCE of each disposition; this guards only the COMPLETENESS of the
// published per-finding record, which nothing else covers.
async function verifyClaudeSection(round, openIds) {
  const path = claudeSectionFile(round)
  // Each open finding must appear as a backticked id token (e.g. `F1`) in the
  // section. `grep -F` is a literal substring match — no regex/prose brittleness.
  const idChecks = openIds
    .map((id) => `grep -Fq ${shq(`\`${id}\``)} ${shq(path)} || { echo ${shq(`missing-${id}`)}; ok=0; }`)
    .join('; ')
  const idList = openIds.length ? openIds.map((id) => `\`${id}\``).join(', ') : '(none)'
  const res = await agent(
    `You are a MECHANICAL RUNNER. Run exactly this and nothing else, then report:\n\`ok=1; test -s ${shq(path)} || { echo empty; ok=0; }${idChecks ? `; ${idChecks}` : ''}; echo "ok=$ok"\`\nThis checks the section file exists, is non-empty, and contains a backticked marker for every open finding (${idList}). Return \`ok\`: true if the final line was "ok=1", false otherwise (any "empty" or "missing-Fn" line means false). Do not edit any file. Do not run git.`,
    {
      label: `verify:claude:round${round}`,
      phase: 'Debate',
      model: mechModel,
      schema: { type: 'object', additionalProperties: false, required: ['ok'], properties: { ok: { type: 'boolean', description: 'true when the section file exists, is non-empty, and carries a backticked marker for every open finding' } } },
    },
  )
  return res?.ok === true
}

const transcript = []
// 'consensus' is the only NORMAL terminus. 'reviewer-error' is the one abnormal
// terminus: codex itself failed to produce a verdict (broken/unavailable). That
// is infrastructure failure, not a debate outcome, so it ends the loop too —
// distinct from the deliberate "no deadlock exit" for substantive disagreement.
let status = 'consensus'
let finalVerdict = null
// The author's PRIOR-round disposition section file — fed to codex next round as
// the rebuttal (codex-review.sh cats it into codex's prompt). null until the first
// author turn writes one. This replaces the old in-memory rebuttal blob: the author
// writes the file itself, so the dispositions never round-trip through structured
// output, and codex reads them straight off disk.
let lastClaudeSectionPath = null
// Rounds where the author edited files (commit mode on) but returned no SHA —
// the in-session commit it was told to make didn't land. The edits aren't lost
// (they stay in the tree and the next reviewer still diffs them against base),
// but the "one commit per round" contract was broken for that round, so the run
// is NOT a clean consensus: we downgrade the terminal status below rather than
// report success over a missed commit. Not a hard abort: a transient SHA omission
// shouldn't nuke a multi-round debate whose edits are all present in the tree.
const commitGaps = []
// Rounds where the author's disposition section file is missing, empty, OR missing
// a backticked marker for one or more open findings after its turn — the handoff
// that feeds the rebuttal, the author's memory, and the comment broke. We DON'T
// silently let an empty or per-finding-incomplete rebuttal slip to codex (which
// would warn and proceed, possibly converging over a hole in the trail): a miss is
// recorded here and downgrades the terminal status to 'section-incomplete' below.
// Not a hard abort for the same reason as commitGaps — the tree edits are still
// present and reviewed.
const sectionGaps = []

// ---------------------------------------------------------------------------
// The loop — runs until consensus. No round cap, no deadlock exit.
// ---------------------------------------------------------------------------
// The debate continues, round after round, until codex resolves every finding
// (any severity). No upper bound, no "deadlock" surrender: the two sides argue
// every point until one concedes. (The harness's per-workflow agent backstop is
// the only hard ceiling; interrupt via /workflows or TaskStop by hand.)
phase('Debate')

// Resolve the diff base to the merge-base of (base, HEAD) so codex reviews only
// what THIS branch changed, not commits the base branch gained since the branch
// forked (those would otherwise show up in `git diff base` — master's drift
// reviewed as ours). A thin mechanical git agent; the workflow can't run git
// itself. Idempotent when `base` is already a merge-base SHA (caller resolved it).
const rawBase = base
const baseRes = await agent(
  `You are a MECHANICAL RUNNER. Run \`git -C ${repoPath} merge-base ${base} HEAD\` and return ONLY the resulting commit SHA (hex) in \`sha\`. If the command FAILS (missing/typoed base, stale ref, unrelated history), return \`sha\`: "" and put the verbatim git error in \`error\` — do NOT fall back to the raw base ref. Do nothing else.`,
  { label: 'resolve:merge-base', phase: 'Debate', model: mechModel, schema: { type: 'object', additionalProperties: false, required: ['sha'], properties: { sha: { type: 'string', description: 'the merge-base SHA, or "" on failure' }, error: { type: 'string', description: 'the git error when sha is empty' } } } },
)
// Fail loud on a bad base. Falling back to the raw `${base}` tip would review the
// base branch's drift since the fork as if this change made it — the exact noise
// the merge-base removes — so a missing/typoed/stale base must abort, not degrade.
if (!baseRes?.sha?.trim()) {
  const err = (baseRes?.error || '').trim()
  log(`Aborting: \`git merge-base ${rawBase} HEAD\` failed; the diff scope can't be trusted. Not falling back to the raw ${rawBase} tip.`)
  return {
    status: 'merge-base-error',
    base: rawBase,
    rounds: 0,
    transcript: [],
    finalVerdict: null,
    note: `merge-base of \`${rawBase}\` and HEAD could not be resolved (missing/typoed base, stale ref, or unrelated history), so the review scope is untrustworthy. Fix the base ref (e.g. \`git fetch\`) and re-run.${err ? `\ngit error:\n${err}` : ''}`,
  }
}
base = baseRes.sha.trim()
log(`Diffing against ${base.slice(0, 12)} (merge-base of ${rawBase} and HEAD), so the base branch's drift since the fork isn't reviewed.`)

// Clear any stale ledger from a PRIOR debate in this worktree. The scratch dir
// is persistent (per-worktree, not per-run) and the section files use a flat,
// stable `section-NNN-*.md` namespace, so a previous longer debate's high-numbered
// sections would otherwise survive into this run — the author cats `section-*.md`
// as its memory, and the orchestrator cats them into the posted comment, so stale
// sections would pollute BOTH the author's context and the published trail. (The
// glob also catches a prior author's claude section files, which double as the
// rebuttal codex reads, so a stale one must not linger either.) A thin mechanical
// agent (the workflow can't run shell itself). The reset is section/ledger-scoped:
// it deletes only the stale `section-*.md` files, not the whole scratch dir, so
// other artifacts in there (verdict-N.json and any other per-run files) keep their
// own lifecycle and a future pre-loop writer won't be silently wiped. This script
// has no true resume (agent() is one-shot, the whole workflow re-runs from
// scratch), so a fresh start owns a fresh ledger.
await agent(
  `You are a MECHANICAL RUNNER. Run exactly this and nothing else: \`mkdir -p -- ${shq(workDir)} && rm -f -- ${shq(workDir)}/section-*.md\`. Do not edit any other file. Do not run git.`,
  { label: 'ledger:reset', phase: 'Debate', model: mechModel },
)

// Persist the author's rationale ONCE (it's constant across rounds) so
// codex-review.sh can inject it into codex's round-1 prompt; codex's warm session
// then carries the note across later rounds without re-injection. Only when a
// rationale was passed — otherwise rationaleFileArg is `-` and no file is needed.
if (rationale) {
  await writeFileAgent(rationaleFile, rationale, 'rationale:write')
}

for (let round = 1; ; round++) {
  const verdict = await codexReviews(round, lastClaudeSectionPath)
  finalVerdict = verdict
  const entry = { round, codex: verdict, claude: null }
  transcript.push(entry) // record this round (mutated in place as it progresses)

  // Write codex's section for this round to disk straight away — before any
  // terminal break — so EVERY round (including a consensus-approval or error round
  // that never reaches the author) lands in the section record the author reads as
  // memory and the orchestrator cats into the comment. The claude section is the
  // author's own write later in the round, when there is an author turn.
  await writeCodexSection(round, verdict)

  // Reviewer error — terminal failure path. The runner could not get a verdict
  // out of codex (broken/unavailable CLI), so codex-review.sh synthesized an
  // error verdict carrying reviewerError:true. There are no findings to route to
  // Claude, and retrying a broken reviewer just spins forever, so abort the
  // debate and surface the failure. This is deliberately separate from the
  // "no deadlock exit" rule, which only governs substantive disagreement.
  if (verdict.reviewerError) {
    status = 'reviewer-error'
    log(`Round ${round}: reviewer error — aborting debate. ${verdict.summary}`)
    break
  }

  const open = openFindings(verdict)
  log(`Round ${round}: codex approved=${verdict.approved}, findings open=${open.length}`)

  // Consensus requires BOTH no open finding AND codex's explicit approval. An
  // inconsistent verdict — `approved:false` with nothing open — is not consensus:
  // codex declined to approve while leaving us nothing to route to Claude, so
  // treating it as agreement would ship an unapproved change. There's no finding
  // to debate, so re-running codex would just replay the same inconsistency;
  // surface it as a reviewer error (the terminal abnormal path) instead of
  // looping forever or falsely converging.
  if (open.length === 0 && verdict.approved !== true) {
    status = 'reviewer-error'
    log(`Round ${round}: inconsistent verdict — approved=false with no open findings; aborting as reviewer-error.`)
    break
  }

  // Consensus: codex approved AND every finding resolved (any severity).
  if (open.length === 0) {
    break
  }

  // Claude responds: fixes what it agrees with (editing the tree), disputes the
  // rest, and writes its dispositions to its own claude section file (its memory,
  // the rebuttal codex reads next round, and part of the posted comment). It reads
  // the per-round section files for its cross-round memory. We point the rebuttal at
  // the path it just wrote so the NEXT round feeds it to codex — but only AFTER
  // verifying the file actually landed (see verifyClaudeSection below).
  const response = await claudeResponds(round, verdict, commit)
  entry.claude = response
  log(
    `Round ${round}: claude done=${response.done}, files=${(response.filesChanged || []).length}`,
  )

  // VERIFY the author wrote its disposition section before we lean on it. The file
  // is the next round's rebuttal, the author's memory, and part of the comment — if
  // it's missing/empty/incomplete the handoff broke. We require a backticked marker
  // for every finding the author had to address this round (`open`), so a non-empty
  // but partial section can't slip a per-finding hole into the trail. Only point
  // `lastClaudeSectionPath` at it (so codex reads it as the rebuttal next round) once
  // it exists AND covers every open id; on a miss record the gap, leave the rebuttal
  // pointer where it was (codex sees `-`/the prior round's section rather than an
  // incomplete one), and downgrade the terminal status.
  if (await verifyClaudeSection(round, open.map((f) => f.id))) {
    lastClaudeSectionPath = claudeSectionFile(round)
  } else {
    sectionGaps.push(round)
    log(`Round ${round}: author's disposition section ${claudeSectionFile(round)} is missing, empty, or missing a marker for an open finding — handoff broke; not feeding it to codex as the rebuttal.`)
  }

  // The author commits its own round in-session (one commit per round, message
  // carrying codex's findings and its dispositions), so here we just record the
  // SHA it returned. Only when it actually changed files; flag the inconsistency
  // if it reported changes but no commit rather than silently dropping it.
  if (commit && (response.filesChanged || []).length > 0) {
    entry.commit = (response.commitSha || '').trim()
    if (entry.commit) {
      log(`Round ${round}: committed ${entry.commit}`)
    } else {
      // The author edited the tree but didn't return a SHA: its in-session commit
      // didn't land. Record the gap so the terminal status reflects it instead of
      // reporting a clean consensus over a round that broke the one-commit-per-round
      // contract. The edits themselves remain in the tree for the next reviewer.
      commitGaps.push(round)
      log(`Round ${round}: author changed ${response.filesChanged.length} file(s) but returned no commit SHA — round left uncommitted`)
    }
  }
  // No section write here: codex's section was written at the top of the loop, and
  // the author wrote its own claude section during its turn — both already on disk.
}

const filesChanged = Array.from(
  new Set(transcript.flatMap((e) => (e.claude && e.claude.filesChanged) || [])),
)

// Downgrade a would-be consensus when any round's in-session commit didn't land.
// The debate may have converged (codex approved, nothing open), but with the
// "one commit per round" contract broken we must NOT advertise a clean consensus:
// /be-review keys off this status (and the SKILL's status table) to decide whether
// the step settled cleanly. 'commit-incomplete' is a distinct, non-consensus
// terminus — the edits are all in the tree (the next reviewer diffs them), but a
// human/caller must reconcile the uncommitted round(s). We don't touch a status
// that's already abnormal (reviewer-error), which is strictly more severe.
if (status === 'consensus' && commitGaps.length) {
  status = 'commit-incomplete'
  log(`Round(s) ${commitGaps.join(', ')} left uncommitted despite changing files — downgrading consensus to commit-incomplete.`)
}

// Downgrade a would-be consensus when any round's author skipped its disposition
// section file OR left it missing a marker for an open finding. The debate may read
// as converged, but a missing or per-finding-incomplete section is a hole in the
// trail the author, codex (as the rebuttal), and the posted comment all draw on —
// so we must NOT advertise a clean consensus. 'section-incomplete' is a distinct,
// non-consensus terminus: a human/caller must fill in the missing round(s) before
// trusting the per-round record. We don't override an already-abnormal status
// (reviewer-error, or commit-incomplete which is reported the same converged-but-
// -not-clean way) — the first downgrade already marks the run unclean.
if (status === 'consensus' && sectionGaps.length) {
  status = 'section-incomplete'
  log(`Round(s) ${sectionGaps.join(', ')} are missing the author's disposition section or a finding marker — downgrading consensus to section-incomplete.`)
}

log(`Debate ended: ${status} after ${transcript.length} round(s); ${filesChanged.length} file(s) changed.`)

// The terminal round needs no extra section write: codex's section for it was
// written at the top of the loop (every round), and a terminal round has no author
// turn (and so no claude section) by definition.

// Hand the orchestrator everything it needs to post the comment, but NOT a single
// pre-rendered `comment` string — the author's per-round dispositions live in the
// section files on disk (it wrote them itself), and the workflow can't read files.
// So we return the small in-process `commentHeader` plus the section dir + glob; the
// orchestrator assembles the comment with a faithful `cat` (header followed by the
// section files in glob order) and posts that — a deterministic shell concat, no
// agent ever retyping the ledger. See SKILL step 3.
return {
  status,
  rounds: transcript.length,
  base,
  finalVerdict,
  filesChanged,
  // Rounds whose author-side commit didn't land (empty unless status is
  // 'commit-incomplete'). Lets the caller pinpoint and reconcile the gap.
  commitGaps,
  // Rounds whose author-side disposition section file is missing/empty (empty unless
  // status is 'section-incomplete'). Lets the caller pinpoint the hole in the trail.
  sectionGaps,
  transcript,
  // The comment's deterministic header (badge + round count + reasoning effort +
  // base). The orchestrator posts: this header, a blank line, then
  // `cat <workDir>/section-*.md`.
  commentHeader: ledgerHeader({ status, rounds: transcript.length, base, reasoningEffort: REASONING_EFFORT }),
  // Where the per-round section files live, so the orchestrator can cat them.
  workDir,
  sectionGlob: `${workDir}/section-*.md`,
}
