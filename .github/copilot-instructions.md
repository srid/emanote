## Emanote

- See architecture.md for high-level of Emanote's code architecture

## Development ennvironment

- The terminal should already be in direnv (with `nix develop` activated). You can also use `nix develop -c ...` to explicility run commands in the Nix shell. Never use legacy commands (like `nix-shell`).

## Your behaviour

- Don't summarize the changes you make (to me, they are self-evident from diff); be brief. 
- If working on a non-master branch, and change involves Github Workflow files: accept and commit and push your changes, then check if the branch workflow succeeded. If not rinse and repeat.
  - Don't sleep for long (60 seconds is too much). Adapt to the workflow speed.

## GitHub Workflow

- Use `gh` with `export GH_PAGER=` to disable pagination.