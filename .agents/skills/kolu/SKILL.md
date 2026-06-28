---
name: kolu
description: >-
  Drive one AI agent from another through kolu's terminals: spawn a Claude
  Code / Codex / opencode session in a PTY, prompt it, watch the screen for its
  reply, read it, and prompt again — a create→send→snapshot loop run with the
  `kaval-tui` CLI directly (no MCP). `kaval-tui` writes input and reads
  scrollback; `pulam-tui` adds a precise agent-state done-signal when you drive
  hooked terminals. Triggers on "drive another agent", "send a prompt to a
  terminal agent", "have one agent prompt another", "agent drives agent",
  "orchestrate agents in terminals", "make Claude drive Codex", "prompt the
  agent running in that terminal", or wiring a loop where one coding agent
  supervises another.
---

# kolu — drive one agent from another through its terminal

You can run a coding agent (Claude Code, Codex, opencode) inside a kolu-owned
PTY and steer it from the outside: type a prompt, submit it, watch the screen
until it's done, read what it said, type the next prompt. The whole toolkit is
**`kaval-tui`** — write input, read the screen, spawn, kill. The driver runs it
directly; there's no server to stand up and no MCP layer.

`pulam-tui` adds a *precise* done-signal (`wait --until <state>`) — but only for
**hooked** terminals (see the last section). For a raw `kaval-tui`-spawned
terminal, the done-signal is **watching the screen settle**, below.

## The loop

```sh
id=$(kaval-tui create --json -- claude | jq -r .id)            # spawn the inner agent
kaval-tui send  "$id" "refactor the parser to use a lexer"     # 1. TYPE the prompt
kaval-tui send  "$id" --key Enter                              # 2. SUBMIT it (its own step)
wait_until_settled "$id"                                       # 3. let its turn finish (below)
kaval-tui snapshot "$id" --viewport                            # 4. read the screen
kaval-tui send  "$id" "now add tests for it"; kaval-tui send "$id" --key Enter   # loop
```

Leaf commands, all `kaval-tui`: **create** (spawn) · **send** (type) ·
**send --key Enter** (submit) · **snapshot** (read) · **kill**. **Typing and
submitting are two separate `send`s** — that is load-bearing, see below.

> **Read with `snapshot --viewport`, not `| tail`.** A bare `snapshot` prints the
> **whole scrollback** — thousands of lines on a long-running or compacted agent —
> so `snapshot | tail -8` hands you the bottom of the buffer (often just trailing
> blanks), not the live screen. `--viewport` asks the daemon for just its
> terminal's last screenful — the right "what's on screen now" read, and correct
> regardless of how tall your own shell is (over `--host` the remote terminal is a
> different size). `--tail N` (alias `--lines N`) bounds it to the last N lines
> when you want a fixed slice.

## `kaval-tui send` — type, then submit (two steps)

`kaval-tui send <id> [text...]` writes input to the terminal — **exactly the text
(and any `--key`s) you pass, with NO implicit Enter**. It types; it does not
submit. **Submitting a prompt is its own second `send`:**

```sh
kaval-tui send "$id" "fix the failing test in parser.ts"   # 1. type the prompt
kaval-tui send "$id" --key Enter                           # 2. submit it
```

**Do this as two separate `send` commands — not `send "text" --key Enter` in one
call.** The separation is load-bearing: an Enter sent in the same breath as the
text races Claude Code's bracketed-paste / debounced input handling — it arrives
before the pasted text has registered and is **silently dropped**, leaving the
prompt staged on the `❯` line while `send` reports success. A standalone
follow-up `send --key Enter` lands after the text has settled, so it actually
submits. (If a turn never seems to start, this is the #1 cause — `snapshot` and
look for the prompt sitting unsent on the `❯` line.)

Specifics:

- **Multiline prompts and piped stdin go as one bracketed paste**, so they land
  in the input box as a block instead of submitting line-by-line. Automatic
  (`--paste` / `--no-paste` force it). For a big prompt, pipe it —
  `cat task.md | kaval-tui send "$id"` — then `send "$id" --key Enter`.
- **`--key <name>`** (repeatable, sent after the text) is both the submit channel
  (`Enter`) and the control channel: `Escape`, `C-c`, `Enter`,
  `Up`/`Down`/`Left`/`Right`, `Tab`, `Home`, `End`, `Backspace`, `M-<char>`.
- **`--json`** → `{ id, bytes, paste, keys }` to confirm what was written.

**`send` is blind** — it writes whether or not the agent is ready for input.
Always pair it with `snapshot` so you don't fire a prompt into a not-yet-ready
session (e.g. before the TUI has drawn its input box, or over a trust prompt).

**Interrupt a runaway** before redirecting it:

```sh
kaval-tui send "$id" --key Escape          # stop Claude Code mid-stream
kaval-tui send "$id" --key C-c             # SIGINT whatever's running
```

## The done-signal — watch the screen settle

After you submit, you need to know when the turn ends. For a raw
`kaval-tui`-spawned terminal there's no agent-state feed, so use the screen
itself: **poll `snapshot` until it stops changing.** A working agent streams
output, so a screen that's held still for a couple of polls means the turn ended
— it finished, *or* it's blocked asking you something (both mean "your move").
Then read the snapshot to see which, and respond.

```sh
# Block until <id>'s screen is unchanged across 2 polls, capped at a deadline so
# a wedged agent can't hang the loop forever (the manual equivalent of a timeout).
wait_until_settled() {
  local id=$1 deadline=$(( $(date +%s) + 600 )) prev="" cur stable=0
  while [ "$stable" -lt 2 ] && [ "$(date +%s)" -lt "$deadline" ]; do
    sleep 3
    cur=$(kaval-tui snapshot "$id" --viewport)   # diff the live screen, not the whole scrollback
    if [ "$cur" = "$prev" ]; then stable=$((stable + 1)); else stable=0; fi
    prev=$cur
  done
}
```

Poll **`--viewport`**, not a bare `snapshot`: a settle test diffs two reads, and
diffing two full-scrollback dumps is slow and noisy (any scrollback churn reads
as "still moving"), while two screenfuls compare cleanly.

Tune `sleep`/deadline to the work. It's coarser than `pulam-tui wait` (a busy
agent that pauses mid-thought can read as settled), so after it returns,
**confirm the reply is actually present** in the snapshot before moving on.

## `pulam-tui wait` — the precise done-signal (hooked terminals only)

When you *do* have agent-state detection, `pulam-tui wait <id> --until <buckets>`
is the exact done-signal — it blocks until the agent reaches a coarse state, then
exits 0:

- **`working`** — busy (`thinking` / `tool_use` / background task).
- **`awaiting`** — `awaiting_user`: it's **asking you** a question.
- **`waiting`** — the **just-finished** post-turn lull.

`awaiting` and `waiting` both mean "your move", so `--until awaiting,waiting`
catches a turn ending; `--timeout <ms>` fails loud (exit 2) so a wedged agent
can't hang the loop; if the terminal **exits** before reaching the state, `wait`
fails loud too (exit 3 — the agent you were driving died); `--json` →
`{ id, agent }`.

> **Mind the stale-state race — wait in two phases.** `wait` matches the agent's
> state **the instant it connects**, replaying whatever it is right now. So right
> after a `send`, the agent may still report the *previous* turn's
> `waiting`/`awaiting` for a beat before it picks up the new prompt — and a lone
> `wait --until awaiting,waiting` would return immediately on that stale state,
> before the turn you asked for has even begun. For a robust loop, wait for the
> pickup first, then the turn-end:
>
> ```sh
> kaval-tui send "$id" "fix the parser"; kaval-tui send "$id" --key Enter
> pulam-tui wait "$id" --until working           # 1. it picked up the prompt
> pulam-tui wait "$id" --until awaiting,waiting   # 2. its turn ended
> ```

> **Caveat — agent state needs HOOKED terminals.** Detection keys on kolu's shell
> rc-hooks (the OSC marks a terminal emits as commands run). `kaval-tui create`
> is the **raw** multiplexer — a plain `$SHELL`, no hooks by design — so an agent
> you spawn that way often isn't detected, and `wait` will just time out. `wait`
> is reliable when you drive **already-hooked** terminals: the ones a running
> **kolu-server** spawned (point `kaval-tui --socket $XDG_RUNTIME_DIR/kolu/pty-host.sock`
> at them), or a future `kolu-tui`. For a raw `kaval-tui create` loop, use the
> screen-settle done-signal above.

## Reach — which daemon you're driving

Bare `kaval-tui` **autodiscovers** a running daemon on this machine. Two ways to
point it elsewhere:

- **`--socket <path>`** targets a specific local daemon — e.g. a running
  **kolu-server's** kaval (`$XDG_RUNTIME_DIR/kolu/pty-host.sock`), to drive the
  terminals you have open in kolu (these ARE hooked, so `pulam-tui wait` works
  against them once a `pulam` reads that kaval).
- **`--host <ssh>`** reaches a daemon on another machine (provisioned with Nix);
  a remote PTY survives the link.

## Acceptance

Before calling a driven turn done:

- You **submitted with a separate `send --key Enter`** (not an implicit Enter,
  not `send "text" --key Enter` in one call) — a prompt left staged on the `❯`
  line is the #1 failure here.
- The inner agent's **reply is actually in the `snapshot`** — not an empty box or
  a half-rendered stream. The screen-settle wait is coarse; verify the content.
- Your wait had a **deadline / `--timeout`** so a wedged agent fails instead of
  hanging the loop.
- If the screen settled on a **question** (the agent is awaiting you), you **read
  it and answered** — you didn't send the next task on top of a blocked prompt.
