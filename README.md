# emabook

WIP: Spiritual successor to [neuron](https://neuron.zettel.page), based on [Ema](https://ema.srid.ca).

## Getting Started

To develop with full IDE support in Visual Studio Code, follow these steps:

- [Install Nix](https://nixos.org/download.html) & [enable Flakes](https://nixos.wiki/wiki/Flakes)
- Run `nix-shell --run haskell-language-server` to sanity check your environment 
- [Open as single-folder workspace](https://code.visualstudio.com/docs/editor/workspaces#_singlefolder-workspaces) in Visual Studio Code
    - Install the recommended extensions
    - <kbd>Ctrl+Shift+P</kbd> to run command "Nix-Env: Select Environment" and select `shell.nix`. The extension will ask you to reload VSCode at the end.
- Press <kbd>Ctrl+Shift+B</kbd> in VSCode, or run `bin/run` (`bin/run-via-tmux` if you have tmux installed) in terminal, to launch the Ema dev server, and navigate to http://localhost:9010/

All but the final step need to be done only once. Check [the Ema tutorial](https://ema.srid.ca/start/tutorial) next.

## Tasks

- [ ] Default template: should be builtin, obviating `./.emabook`
  - Include them in the Nix install, and reference when running against a notebook without `./.emabook` directory
  - Must include things like prismJS syntax highlighting
- [ ] Milestone: `./emabook ~/code/haskell-knowledge-base` should just work.
- [ ] Make repo public

To triage,

- [ ] Remove hardcoded Tailwind based HTML for sidebar & breadcrumbs
- [ ] Remove hardcoding of CSS classes in Pandoc's HTML
  - If using Tailwind, requires `@apply` somehow. Can be addressed using twind's preflight.
- [ ] Redirect to README.md if there is no index.md
- [ ] ... more in my notebook.


Before public release

- [ ] Finalize on the project name: `emabook`, or something else?