---
paths:
  - "**"
---

# Developing Emanote

Expect humans to use /do to start work.

## Internationalisation

Emanote sites may use a single language, but that language is not necessarily English. When adding or changing user-facing strings in default templates, default JavaScript, or site-authored JavaScript, treat the change as i18n work:

1. Keep literal fallback strings in code only as fallbacks.
2. Add or update the canonical string keys under `template.i18n` in `emanote/default/index.yaml` for every built-in language.
3. In JavaScript, read strings through `_emanote-static/js/i18n.js` helpers such as `text` or `message` instead of hard-coding visible UI text.
4. Add or update e2e coverage when the string is part of visible default UI chrome.

## Dependencies

Emanote depends on various dependencies (flake.nix inputs). When work requires changes to a dependency:

1. Clone the dependency under `~/code` (if not already there) and create a branch there.
2. Do the work in the dependency repo and open a PR against it.
3. **The dependency PR is not a shortcut.** It MUST go through the full `/do` workflow exactly as Emanote PRs do — this is non-negotiable. In particular:
   - Run `/code-police` on the dependency PR before requesting review.
   - Run `/hickey` and `/lowy` on the dependency PR for structural and boundary review.
   - Iterate on CI and review feedback in the dependency PR until it is green and clean.
4. Only after the dependency PR meets the same quality bar as an Emanote PR, wire it into Emanote by updating `flake.nix` inputs to point at the dependency branch/PR.

Treat every dependency PR as a first-class PR. Skipping `/code-police`, `/hickey`, or `/lowy` on a dependency PR is a workflow violation, even if the change looks small.
