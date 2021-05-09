# Ema Template

This repository represents a **real-world example** of [Ema](https://ema.srid.ca/) — it is the source for ema.srid.ca website — and as such acts as a **template repository** to use for bootstrapping your next static site using Ema.

## Getting Started

To develop with full IDE support in Visual Studio Code, follow these steps:

- [Install Nix](https://nixos.org/download.html) & [enable Flakes](https://nixos.wiki/wiki/Flakes)
- Run `nix-shell --run haskell-language-server` to sanity check your environment 
- [Open as single-folder workspace](https://code.visualstudio.com/docs/editor/workspaces#_singlefolder-workspaces) in Visual Studio Code
    - Install the recommended extensions
    - <kbd>Ctrl+Shift+P</kbd> to run command "Nix-Env: Select Environment" and select `shell.nix`. The extension will ask you to reload VSCode at the end.
- Press <kbd>Ctrl+Shift+B</kbd> in VSCode, or run `bin/run` (`bin/run-via-tmux` if you have tmux installed) in terminal, to launch the Ema dev server, and navigate to http://localhost:9001/

All but the final step need to be done only once. Check [the Ema tutorial](https://ema.srid.ca/start/tutorial) next.

## Note

- This project uses [relude](https://github.com/kowainik/relude) as its prelude, as well as [Tailwind+Blaze](https://ema.srid.ca/guide/helpers/tailwind) as CSS utility and HTML DSL. Even though the author highly recommends them, you are of course free to swap them out for the library of your choice.
- As a first step to using this template, 
  - change the project name in .cabal, flake.nix and hie.yaml files.
  - Change the `cname` in .github/workflows/publish.yaml, or remove it to publish to yourname.github.io
- Configuration:
  - To change the port, see file bin/run
  - To change the CLI arguments used by bin/run, see file .ghcid
  - To update Ema to latest Git revision, run `nix flake lock --update-input ema`
  - To add/remove Haskell dependencies, see the .cabal file. If a dependency is unavailable in nixpkgs, you can override it (to point to say a Git repo) in the `overrides` attribute of flake.nix. You can imitate the manner in which the `ema` (or `lvar`) package itself is overriden.
- To generate static site, run: `nix run . -- -C ./content gen ./output`
