## Emanote

- See architecture.md for high-level of Emanote's code architecture

## Your behaviour

- Don't summarize the changes you make (to me, they are self-evident from diff); be brief. 

## GitHub Workflow

- Use `gh` with `export GH_PAGER=` to disable pagination when using `gh`.

## Nix

- The VSCode terminal is usually in the Nix devShell already (activated via direnv). `cabal` and the like are available. If not, you can run `nix develop -c ...`.