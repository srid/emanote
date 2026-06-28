export const meta = {
  name: 'codex-answer-debate',
  description: 'Have Claude and codex each answer a prompt in parallel, then cross-check until they agree, and synthesize one unified answer (no round cap, no deadlock exit)',
  phases: [
    { title: 'Answer', detail: 'claude + codex answer the prompt independently, in parallel' },
    { title: 'Reconcile', detail: 'each cross-checks the other, round after round, until both agree' },
    { title: 'Synthesis', detail: 'merge the two agreed answers into one unified reply' },
  ],
}

// ---------------------------------------------------------------------------
// Inputs (passed via the Workflow tool's `args`)
// ---------------------------------------------------------------------------
// The harness JSON-ENCODES `args` before the workflow sees it, so it arrives as a
// STRING even when the caller passed a real object; a bare `args.repoPath`/`.prompt`
// would then be `undefined` and every input silently default. Parse a stringified
// `args` defensively (empty string → {}; object used as-is; malformed JSON throws
// loudly). See debate.workflow.js for the full cross-repo failure this fixes.
const a = typeof args === 'string' ? (args.trim() ? JSON.parse(args) : {}) : args || {}
const repoPath = a.repoPath || '.'
// The user's freeform prompt — the question both assistants answer and then
// cross-check toward one agreed reply. Required; the orchestrator passes it.
const prompt = (a.prompt || '').trim()
// Where the generated skill lives, so the codex runner can find codex-answer.sh.
const skillDir = a.skillDir || '.claude/skills/codex-debate'
// Per-worktree scratch dir, shared with the review mode. Gitignored, derived from
// repoPath (the worktree root === $PWD) so parallel debates in DIFFERENT worktrees
// never collide on shared /tmp paths and these files never pollute the repo.
const workDir = `${repoPath}/.codex-debate`

// Model tiers. The claude-answer round does real reasoning (answering, then
// cross-checking codex) → `model` (Opus). The final synthesis is also user-facing
// prose, so it runs on `model` too. The codex RUNNER and the transcript writer must
// relay text faithfully (a verbatim copy, not a paraphrase — the weakest tier
// corrupts it silently) → `copyModel` (Sonnet). Defaults match a direct invocation.
const model = a.model || 'opus'
const copyModel = a.copyModel || 'sonnet'

// The reasoning effort codex runs at, scoped to the debate. This JS constant is
// the SINGLE home for the value: it is passed script-ward (a 4th positional arg to
// codex-answer.sh, which sets `-c model_reasoning_effort`) and read by the
// transcript header, so the `-c` flag and the header both derive from here.
const REASONING_EFFORT = 'xhigh'

// POSIX single-quote a path for safe interpolation into a shell command (spaces,
// globs, metacharacters inert; embedded single quotes escaped via '\'' ). Used for
// the destructive scratch reset (`rm -f`) below.
const shq = (s) => `'${String(s).replace(/'/g, `'\\''`)}'`

// A filesystem-safe slug for this prompt, so the saved transcript has a readable,
// deterministic name (Date.now()/Math.random() are unavailable in workflow scripts,
// so the name is derived purely from the prompt text). Falls back to 'answer'.
const slug =
  (prompt.toLowerCase().match(/[a-z0-9]+/g) || []).slice(0, 8).join('-').slice(0, 60) || 'answer'
const answerDocPath = `${workDir}/answer-${slug}.md`

// ---------------------------------------------------------------------------
// Schema — shared by both debaters (codex's runner mirrors
// scripts/codex-answer.schema.json). `reviewerError` is set ONLY by the codex
// runner script when codex itself failed; the claude side never sets it.
// ---------------------------------------------------------------------------
const OBJECTION = {
  type: 'object',
  additionalProperties: false,
  properties: { point: { type: 'string' }, reason: { type: 'string' } },
  required: ['point', 'reason'],
}
const ANSWER_SCHEMA = {
  type: 'object',
  additionalProperties: false,
  properties: {
    answer: { type: 'string' },
    keyPoints: { type: 'array', items: { type: 'string' } },
    agreesWithOther: { type: 'boolean' },
    objections: { type: 'array', items: OBJECTION },
    changedMind: { type: 'string' },
    reviewerError: { type: 'boolean' },
  },
  required: ['answer', 'keyPoints', 'agreesWithOther', 'objections', 'changedMind'],
}
const FINAL_SCHEMA = {
  type: 'object',
  additionalProperties: false,
  properties: { answer: { type: 'string' } },
  required: ['answer'],
}

if (!prompt) {
  return {
    status: 'no-prompt',
    rounds: 0,
    prompt,
    transcriptPath: null,
    finalAnswer: null,
    note: 'No prompt was passed to the answer-debate workflow; nothing to answer.',
  }
}

// ---------------------------------------------------------------------------
// The two debaters (symmetric: each answers, then cross-checks the other)
// ---------------------------------------------------------------------------
// CLAUDE answers/cross-checks via a harness subagent (Claude isn't headless under
// Max auth, so agent() is the only way to run it). Read-only: it may inspect the
// repo to ground its answer but must not edit anything.
async function claudeAnswers(round, other, myPrev) {
  const block =
    round === 1
      ? `Answer the question below thoroughly and honestly — your best, most defensible answer. You are in a debate with another assistant ("CODEX") answering the SAME question independently; you'll cross-check each other afterward, so make this strong.`
      : `This is a CROSS-CHECK round. You and CODEX each answered the question; now reconcile toward ONE agreed answer.

Your OWN previous answer (build on it — don't re-derive from scratch):
${JSON.stringify(myPrev, null, 2)}

CODEX's LATEST answer (and its objections to your previous answer) (JSON):
${JSON.stringify(other, null, 2)}

Weigh CODEX's answer against yours:
  - Where CODEX is right and you were wrong or incomplete, UPDATE your answer to match and say what you changed in changedMind.
  - Where CODEX is wrong or has a gap, hold your position and record it under objections with a specific, evidence-backed reason.`
  const prompt_ = `You and CODEX were asked the SAME question. ${block}

You may inspect the repo at \`${repoPath}\` to ground your answer — your shell cwd may be a different worktree, so use \`git -C ${repoPath}\` and absolute paths under it. READ-ONLY: read files, \`git -C ${repoPath} diff/log\`, grep; do NOT edit, create, delete, or run any git write command. Cite file:line for claims about this codebase. If the question isn't about this repo, answer from your own knowledge.

The question:
${prompt}

Return the schema:
  - answer: your complete, self-contained answer as it stands now (on a cross-check round, your UPDATED unified answer).
  - keyPoints: the core claims your answer rests on, one per item.
  - objections: your remaining disagreements with CODEX's latest answer (empty on round 1, and empty once you fully agree).
  - changedMind: what CODEX convinced you to change this round (empty on round 1 or if nothing changed).
  - agreesWithOther: true ONLY when CODEX's latest answer is correct and complete and you have NO objection left — your two answers say the same thing. false on round 1.`
  return agent(prompt_, {
    label: `claude:round${round}`,
    phase: round === 1 ? 'Answer' : 'Reconcile',
    model,
    schema: ANSWER_SCHEMA,
  })
}

// A confirmation/approval turn for ONE side, judging a SINGLE shared candidate
// answer (the synthesized merge) WITHOUT rewriting its own. Both sides judge the
// IDENTICAL text, so no swap/oscillation is possible (the swap hazard exists only
// because each parallel round adopts the OTHER's separate answer). Returns
// `agreesWithOther` + `objections` against that candidate. Claude runs it directly
// (it's read-only reasoning); codex runs it through the same cross-check machinery
// (the candidate is handed to it as "CLAUDE's latest answer" to approve).
async function claudeConfirms(round, candidate) {
  const prompt_ = `You and CODEX were asked the SAME question, debated, and AGREED. A unified candidate answer has been synthesized from your two agreed answers. Your ONLY job now is to APPROVE it or object — do NOT rewrite it, do NOT produce a new answer of your own.

You may inspect the repo at \`${repoPath}\` to verify (READ-ONLY — \`git -C ${repoPath} diff/log\`, read files, grep; do NOT edit/create/delete or run any git write).

The question:
${prompt}

The candidate unified answer to approve:
${candidate}

Return the schema:
  - answer: echo the candidate VERBATIM (you are approving it, not rewriting it).
  - keyPoints: the core claims the candidate rests on.
  - objections: anything the candidate gets wrong, drops, or overstates relative to what you agreed — empty if you approve it as-is. Be specific (file:line for repo claims).
  - changedMind: empty (you are confirming, not revising).
  - agreesWithOther: true ONLY if you approve the candidate as a correct, complete unified answer with NO objection left.`
  return agent(prompt_, {
    label: `claude:confirm${round}`,
    phase: 'Synthesis',
    model,
    schema: ANSWER_SCHEMA,
  })
}

// CODEX answers/cross-checks via codex-answer.sh (warm session across rounds). The
// agent here is a MECHANICAL RUNNER: it writes the prompt + cross-check files,
// shells out to the script, and relays codex's JSON answer verbatim — it does NOT
// answer the question itself. The user's prompt and CLAUDE's latest answer carry
// arbitrary characters, so they're written with the Write tool, never a heredoc.
// On a CONFIRM turn (`confirming`), `other` is the SINGLE shared synthesized
// candidate STRING; the runner writes it to the cross-check file and passes the
// script's `confirm` token so codex plugs into the same verbatim/approve-or-object
// contract as the workflow's claudeConfirms turn (NOT the ordinary cross-check
// contract, which would tell codex to UPDATE its own answer instead of approving).
async function codexAnswers(round, other, confirming) {
  const answerPath = `${workDir}/answer-codex-${round}.json`
  const promptPath = `${workDir}/answer-prompt.txt`
  const crossPath = `${workDir}/answer-crosscheck.json`
  // The cross-check argument to the script: `-` on round 1 (codex answers
  // independently), else the file holding either CLAUDE's latest answer (ordinary
  // cross-check) or the synthesized candidate to approve (confirm turn).
  const crossArg = round === 1 ? '-' : crossPath
  // On a confirm turn the cross-check file holds the candidate VERBATIM (a plain
  // string); on an ordinary cross-check it holds CLAUDE's answer JSON.
  const crossContent = confirming ? other : JSON.stringify(other, null, 2)
  const crossStep =
    round === 1
      ? `2. (No cross-check this round — codex answers independently.)`
      : `2. Using the Write tool (NOT a shell heredoc — the content has special characters), create \`${crossPath}\` with EXACTLY this content (overwriting any existing file):

${crossContent}`
  // Pass the script's `confirm` token on a confirm turn so it selects the
  // approve-the-candidate prompt shape rather than the cross-check shape.
  const confirmArg = confirming ? ` ${shq('confirm')}` : ''
  // Every path below is spliced into a shell command the runner agent executes, so
  // POSIX-quote each one (worktrees or skill dirs with spaces/metacharacters would
  // otherwise break the command or misdirect it). The Write-tool file CONTENTS
  // (${prompt}, ${crossContent}) are not shell-parsed and stay verbatim.
  const runnerPrompt = `You are a MECHANICAL RUNNER for one round of an automated answer-debate. Do exactly the steps below and nothing else. Do NOT answer the question yourself, do NOT edit repository files, do NOT add commentary.

1. Ensure the scratch dir exists: \`mkdir -p ${shq(workDir)}\`. Using the Write tool (NOT a heredoc), create \`${promptPath}\` with EXACTLY this content (overwriting any existing file):

${prompt}

${crossStep}

3. Run (cd into the repo root so the script's internal \`git\` targets THIS worktree — your shell cwd may be a different worktree):
   \`cd ${shq(repoPath)} && bash ${shq(`${skillDir}/scripts/codex-answer.sh`)} ${shq(promptPath)} ${crossArg === '-' ? '-' : shq(crossArg)} ${shq(answerPath)} ${shq(REASONING_EFFORT)}${confirmArg}\`

   This shells out to the codex CLI as a read-only peer; it can take 1-3 minutes. It prints a JSON answer as its final stdout and also writes it to \`${answerPath}\`.

4. Read \`${answerPath}\` and return its exact contents as your structured output. Copy the values faithfully; do not paraphrase or "improve" them.`
  return agent(runnerPrompt, {
    label: confirming ? `codex:confirm${round}` : `codex:round${round}`,
    phase: confirming ? 'Synthesis' : round === 1 ? 'Answer' : 'Reconcile',
    model: copyModel, // must relay codex's answer JSON faithfully
    schema: ANSWER_SCHEMA,
  })
}

// ---------------------------------------------------------------------------
// Transcript rendering — deterministic, in-process (no agent retypes the blob)
// ---------------------------------------------------------------------------
function renderObjections(objs) {
  if (!objs || objs.length === 0) return '  - _(none)_'
  return objs.map((o) => `  - **${o.point}** — ${o.reason}`).join('\n')
}

function renderSide(name, ans) {
  if (!ans) return `**${name}** — _(no turn this round)_`
  const lines = [
    `**${name}** — agrees with other: \`${!!ans.agreesWithOther}\``,
    '',
    ans.answer,
  ]
  if (ans.changedMind && ans.changedMind.trim()) lines.push('', `_changed mind:_ ${ans.changedMind}`)
  lines.push('', 'Objections to the other side:', renderObjections(ans.objections))
  return lines.join('\n')
}

// A confirm round is not a normal answer round: both sides judged ONE shared
// synthesized candidate (approve-or-object), so rendering each side's `answer`
// would misleadingly show two texts for a round whose whole point was that both
// judged the IDENTICAL one. Show the candidate ONCE, then each side's verdict
// (agrees + objections) against it.
function renderConfirmVerdict(name, ans) {
  if (!ans) return `**${name}** — _(no turn this round)_`
  return [
    `**${name}** — approved: \`${!!ans.agreesWithOther}\``,
    'Objections to the candidate:',
    renderObjections(ans.objections),
  ].join('\n')
}

function roundSection(entry) {
  if (entry.confirming) {
    return [
      `### Round ${entry.round} — confirmation`,
      '',
      'Both sides judged this single synthesized candidate (approve or object):',
      '',
      entry.candidate,
      '',
      renderConfirmVerdict('claude', entry.claude),
      '',
      renderConfirmVerdict('codex', entry.codex),
    ].join('\n')
  }
  return [
    `### Round ${entry.round}`,
    '',
    renderSide('claude', entry.claude),
    '',
    renderSide('codex', entry.codex),
  ].join('\n')
}

function transcriptHeader(meta) {
  const badge = meta.status === 'consensus' ? '✅ **Agreed**' : `⚠️ **${meta.status}**`
  return `# Codex ⇄ Claude answer-debate

> **Prompt:** ${meta.prompt}

${badge} after ${meta.rounds} round(s) · codex answered at \`${meta.reasoningEffort}\` reasoning effort`
}

function renderTranscript(transcript, meta, finalAnswer) {
  const parts = [transcriptHeader(meta)]
  if (finalAnswer) parts.push('## Final unified answer', '', finalAnswer)
  parts.push('## Convergence trail', ...transcript.map(roundSection))
  return parts.join('\n\n')
}

phase('Answer')

const transcript = []
let status = 'consensus'
let claudeAns = null
let codexAns = null

// A side "agrees" only when it BOTH set agreesWithOther AND left no objection. The
// boolean and the objections list must be consistent (the schema says so), but a
// model can set the flag while still listing a disagreement; honour the objections
// too so a leftover objection can't be papered over by an over-eager boolean.
const sideAgrees = (a) => a.agreesWithOther === true && (a.objections || []).length === 0

// Synthesize a SINGLE candidate answer from the two agreed answers. This is the
// user-facing unified reply, but it is NOT returned until BOTH sides approve it (the
// confirmation phase below), so the synthesized text is never reported as consensus
// without both debaters having signed off on it. Returns the candidate string, or
// null if the synthesis agent died / returned empty.
async function synthesize(claudeFinal, codexFinal) {
  const synth = await agent(
    `Claude and codex were each asked the question below and, after cross-checking, AGREED. Merge their two (now-equivalent) answers into ONE clean, unified, self-contained answer for the user — no "Claude said / codex said" framing, no meta-commentary about the debate, just the best single answer. Preserve every substantive point both kept; where they used different wording for the same idea, pick the clearest. Keep any file:line citations.

The question:
${prompt}

Claude's final answer (JSON):
${JSON.stringify(claudeFinal, null, 2)}

Codex's final answer (JSON):
${JSON.stringify(codexFinal, null, 2)}

Return the unified answer in \`answer\`.`,
    { label: 'synthesis', phase: 'Synthesis', model, schema: FINAL_SCHEMA },
  )
  return synth && synth.answer ? synth.answer : null
}

// ---------------------------------------------------------------------------
// The loop — round 1 is the independent answer (parallel); rounds 2+ are
// cross-checks. Runs until BOTH sides agree, then a CONFIRMATION phase on a single
// synthesized candidate. No round cap, no deadlock exit: each side keeps
// cross-checking until they converge (the harness's per-workflow agent backstop is
// the only hard ceiling — interrupt via /workflows or TaskStop).
//
// Both sides run in PARALLEL each round, so in round N each cross-checks the OTHER's
// round-(N-1) answer. That parallelism creates a SWAP/OSCILLATION hazard: if Claude
// adopts codex's prior answer while codex simultaneously adopts Claude's prior
// answer, both can report agreesWithOther:true in the SAME round even though their
// CURRENT outputs are swapped and still differ — and they can keep swapping back and
// forth, so counting consecutive parallel agreements does NOT prove the current
// outputs match. The only sound test is to make BOTH sides judge ONE shared piece of
// text. So when a round shows mutual agreement, we synthesize a single candidate
// from the two agreed answers and run a CONFIRMATION phase: both sides review that
// IDENTICAL candidate (without rewriting their own answer) and either approve it or
// object. Approval is on one fixed text both actually saw, so no swap is possible. If
// both approve, that candidate IS the converged answer (already debater-approved). If
// either objects, its objections fold back into the cross-check loop and we continue.
// ---------------------------------------------------------------------------
let finalAnswer = null
// On a confirmation turn, each side judges the SAME synthesized candidate STRING.
// Carried across iterations so a rejected confirmation feeds the candidate + the
// objector's complaints back into the next ordinary cross-check round.
let pendingCandidate = null
for (let round = 1; ; round++) {
  const confirming = pendingCandidate !== null
  const prevClaude = claudeAns
  const prevCodex = codexAns
  // On a confirm turn BOTH sides run their dedicated approve-a-fixed-candidate
  // interface (claudeConfirms / codexAnswers(..., confirming)), so both plug into
  // one verbatim/approve-or-object contract instead of the ordinary cross-check.
  const [claude, codex] = await parallel([
    () =>
      confirming
        ? claudeConfirms(round, pendingCandidate)
        : claudeAnswers(round, round === 1 ? null : prevCodex, prevClaude),
    () =>
      confirming
        ? codexAnswers(round, pendingCandidate, true)
        : codexAnswers(round, round === 1 ? null : prevClaude, false),
  ])

  // codex infrastructure failure — terminal. The runner could not get an answer
  // out of codex (broken/unavailable CLI), so it synthesized reviewerError:true.
  // Retrying a dead reviewer just spins, so abort and surface the failure. This is
  // deliberately separate from the "no deadlock exit" rule for real disagreement.
  if (codex && codex.reviewerError) {
    status = 'reviewer-error'
    log(`Round ${round}: codex error — aborting. ${codex.answer}`)
    transcript.push({ round, claude, codex })
    break
  }
  // A side died on a terminal API error after retries (agent() returned null).
  // We can't reconcile half a debate, so abort loudly rather than loop on nulls.
  if (!claude || !codex) {
    status = 'agent-error'
    log(`Round ${round}: ${!claude ? 'claude' : 'codex'} produced no answer (agent error) — aborting.`)
    transcript.push({ round, claude, codex })
    break
  }

  claudeAns = claude
  codexAns = codex
  // On a confirm round both sides judged the ONE shared candidate; tag the entry
  // (with the candidate itself) so the transcript renders it as an approve/object
  // verdict on a single text rather than as two separate answer rounds.
  const entry = confirming
    ? { round, claude, codex, confirming: true, candidate: pendingCandidate }
    : { round, claude, codex }
  transcript.push(entry)

  // CONFIRMATION phase: both sides judged the SAME synthesized candidate. If both
  // approve it (agreesWithOther:true + no objections), that candidate is the agreed,
  // debater-approved unified answer — converge. If either objects, drop the candidate
  // and continue the cross-check loop (their objections are already in `claudeAns` /
  // `codexAns` and feed the next round).
  if (confirming) {
    if (sideAgrees(claude) && sideAgrees(codex)) {
      finalAnswer = pendingCandidate
      log(`Round ${round}: both sides approved the synthesized candidate — converged.`)
      break
    }
    log(`Round ${round}: candidate rejected (claude agrees=${sideAgrees(claude)}, codex agrees=${sideAgrees(codex)}) — resuming cross-check.`)
    pendingCandidate = null
    continue
  }

  // From round 2 on (round 1 has no cross-check), if BOTH sides report no remaining
  // disagreement, synthesize one candidate and enter the confirmation phase next
  // round. A single parallel-agreeing round can be a swap false positive, so we do
  // NOT converge here — we converge only after both sides approve the SAME candidate.
  const bothAgree = round >= 2 && sideAgrees(claude) && sideAgrees(codex)
  if (bothAgree) {
    phase('Synthesis')
    pendingCandidate = await synthesize(claude, codex)
    if (!pendingCandidate) {
      // The merge itself failed (synthesis agent died / returned empty). The sides
      // DID agree; only the merge broke — surface that explicitly rather than spin.
      status = 'synthesis-error'
      log('Synthesis produced no candidate — both sides agreed but the merge failed; reporting synthesis-error.')
      break
    }
    log(`Round ${round}: both sides agree — synthesized a candidate; confirming it next round.`)
    phase('Reconcile')
  } else {
    log(
      `Round ${round}: claude agrees=${sideAgrees(claude)} (objections=${(claude.objections || []).length}), codex agrees=${sideAgrees(codex)} (objections=${(codex.objections || []).length})`,
    )
  }
}

log(`Answer-debate ended: ${status} after ${transcript.length} round(s).`)

// Persist the full transcript to a single readable file the user can revisit
// (chat + saved transcript). Rendered deterministically in-process, then handed to
// one mechanical writer — the payload can be large, so use copyModel for faithful
// reproduction (the same tier the codex relay uses).
const transcriptText = renderTranscript(
  transcript,
  { status, rounds: transcript.length, prompt, reasoningEffort: REASONING_EFFORT },
  finalAnswer,
)
await agent(
  `You are a MECHANICAL WRITER. Do exactly these steps and nothing else — do not edit any other file, do not run git, do not add commentary.

1. Ensure the scratch dir exists: \`mkdir -p ${shq(workDir)}\`.
2. Using the Write tool, create \`${answerDocPath}\` with EXACTLY this content, overwriting any existing file:

${transcriptText}`,
  { label: 'transcript:write', phase: 'Synthesis', model: copyModel },
)

return {
  status,
  rounds: transcript.length,
  prompt,
  transcriptPath: answerDocPath,
  finalAnswer,
  reasoningEffort: REASONING_EFFORT,
  // The error terminus carries codex's failure detail in the synthesized verdict's
  // answer text. Sourced from the transcript (not codexAns) because the
  // reviewer-error branch breaks BEFORE assigning codexAns — the failing round is
  // recorded in the transcript, so read the detail back from there.
  codexError:
    status === 'reviewer-error'
      ? transcript.find((e) => e.codex && e.codex.reviewerError)?.codex.answer || null
      : null,
}
