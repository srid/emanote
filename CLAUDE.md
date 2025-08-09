## Emanote

- See ./docs/architecture.md for high-level of Emanote's code architecture
- See ./docs for Emanote's user documentation
- The Haskell code is available in ./emanote
- HTML templates and the rest are in ./emanote/default

## Your behaviour

- Don't summarize the changes you make (to me, they are self-evident from diff); instead, be brief. 

## Build and stuff

- Test your Haskell changes by running `cabal build all` In the Nix devshell.
- To test your Nix changes, `git add` any untracked files added by you, then run `nix build` on the relevant package.
  - If the Nix changes belong to a sub-directory, look at the README.me of that directory for additional instructions.