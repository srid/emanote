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

### Testing on haskell-knowledge-base

First, clone [haskell-knowlege-base](https://github.com/tfausak/haskell-knowledge-base) and symlink to Ema's default HTML templates,

```bash
# Clone, and symlink to Ema's default HTML templates
cd ../
git clone git@github.com:tfausak/haskell-knowledge-base.git
cd haskell-knowledge-base/
ln -s ../emabook/docs/.emabook .
ln -s ../emabook/docs/favicon.svg .  # Or use something else
```

Then go back to Emabook, and edit its `.ghcid` file to refer to the haskell-knowledge-base directory instead. It should contain something like this:

```
--warnings -T ":main -C /home/srid/code/haskell-knowledge-base"
```

Finally, run `bin/run` to spin up the server at http://localhost:9010

To generate static files,

```bash
mkdir ./output
nix run . -- -C ../haskell-knowledge-base gen $(pwd)/output
nix-shell -p nodePackages.http-server --run 'http-server ./output/'
```

## Tasks

- [ ] Default template: should be builtin, obviating `./.emabook`
  - Include them in the Nix install, and reference when running against a notebook without `./.emabook` directory
  - Must include things like prismJS syntax highlighting
- [ ] Milestone: `./emabook ~/code/haskell-knowledge-base` should just work.

To triage,

- [ ] Remove hardcoded Tailwind based HTML for sidebar & breadcrumbs
- [ ] Remove hardcoding of CSS classes in Pandoc's HTML
  - If using Tailwind, requires `@apply` somehow. Can be addressed using twind's preflight.
- [ ] Redirect to README.md if there is no index.md
- [ ] ... many more in my private notebook.

Before public release

- [ ] Finalize on the project name: `emabook`, or something else?