## Emanote

- See architecture.md for high-level of Emanote's code architecture

## Your behaviour

- Don't summarize the changes you make (to me, they are self-evident from diff); be brief. 

## Build and stuff

- Test your Haskell changes by running `cabal build all` In the Nix devshell.
- At the *end* of your work, run `just fmt` (auto-format project tree) in the Nix devshell after modifying Haskell or Nix files.