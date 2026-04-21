---
name: github-pr
description: Write engaging GitHub PR titles and descriptions. Use when creating or updating PRs. Avoids boring bullet lists; uses narrative paragraphs with bold/italic for emphasis.
---

# GitHub PR Writing

Write PR descriptions that fellow devs actually want to read.

## Anti-patterns (what LLMs typically produce)

- Flat bullet lists of every file changed
- Implementation-detail dumps ("added `foo` parameter to `bar` function")
- Generic titles like "Update configuration" or "Fix bug in module"
- "## Changes" / "## Testing" / "## Summary" boilerplate headers
- Restating the diff in English

## What to write instead

**Title**: Short, specific, interesting. Convey _what changed from a user/dev perspective_, not which files were touched. Use imperative mood. Under 70 chars.

**Body**: Write in **paragraphs**, not bullet lists. Structure:

1. **Opening paragraph** — What this PR does and _why_, in 2-3 sentences. Bold the key behavioral change. If there's a motivating problem, state it directly.

2. **Details paragraph(s)** — Only if the approach is non-obvious or has trade-offs worth calling out. Use _italics_ for subtle points. Keep it high-level; reviewers can read the diff for implementation details.

3. **Anything notable** — Breaking changes, migration steps, or things reviewers should pay attention to. Only if applicable. Use `> blockquote` for callouts.

## Style rules

- Write for a dev skimming their PR feed — they should get the gist in 5 seconds
- **Bold** the most important phrase in each paragraph
- _Italics_ for nuance, caveats, secondary points
- No bullet lists unless listing 3+ discrete items that genuinely aren't a narrative
- No "## Summary" or "## Changes" headers — just write
- No filler: "This PR...", "In this change...", "As part of..." — start with the substance
- Link to issues/discussions where relevant (`Closes #123`, `See #456`)
- If the PR is trivial (typo fix, version bump), a one-liner body is fine

## Try it locally

If the repo is a Nix flake and the PR branch contains a buildable output (package, NixOS config, etc.), include a "Try it locally" section at the end of the body. Use the GitHub owner/repo and branch name to construct the command:

```
### Try it locally
`nix run github:<owner>/<repo>/<branch>`
```

Adjust the command as needed — `nix build` for non-runnable outputs, add `#<output>` if the default package isn't the relevant one. Omit this section entirely if the change isn't meaningfully testable via `nix run/build` (e.g., CI-only changes, documentation, non-Nix repos).

## Updating existing PRs

When the user pushes further changes to an already-PR'd branch:

1. Check if the PR title/description still accurately reflects the full scope
2. If new commits meaningfully change what the PR does, update the title and/or body via `gh pr edit`
3. Don't rewrite from scratch — amend the existing description to cover new ground
4. Add a brief note about what changed if the scope expanded significantly

## Examples

### Bad (typical LLM output)

```
Title: Update NixOS configuration and add new service

## Summary
- Added `kolu` service configuration
- Updated `flake.lock`
- Modified port from 8080 to 8090
- Added health check endpoint
- Updated README

## Testing
- Tested locally
```

### Good

```
Title: Add kolu service with health monitoring

**Kolu now runs as a standalone NixOS service** with its own systemd
unit and a dedicated health-check endpoint. Previously it was bolted
onto the main app process, which made restarts disruptive.

The service binds to port 8090 to avoid clashing with the dev server.
*Health checks hit `/healthz` every 30s — systemd restarts the
service on three consecutive failures.*
```
