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

All but the final step need to be done only once.

### Testing on haskell-knowledge-base

First, clone [haskell-knowledge-base](https://github.com/tfausak/haskell-knowledge-base) and symlink to Ema's default HTML templates,

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
--warnings -T ":main -C ../haskell-knowledge-base"
```

Finally, run `bin/run` to spin up the server, and go to http://localhost:9010/README

To generate static files,

```bash
mkdir ./output
nix run . -- -C ../haskell-knowledge-base gen $(pwd)/output
nix-shell -p nodePackages.http-server --run 'http-server ./output/'
```

## Tasks

- Splice work
  - [x] Make sidebar tree a splice
  - [ ] Split into modules
  - [x] Make breadcrumbs a splice
    - Requires supporting arbitrary HTML in node children
  - [x] Make pandoc view a splice
- [ ] .emabook/templates/settings.yml - to pass global vars (`theme`, `site-title`) as-is
- [ ] Default template: should be builtin, obviating `./.emabook`
  - Include them in the Nix install, and reference when running against a notebook without `./.emabook` directory
  - Must include things like prismJS syntax highlighting
- [ ] Milestone: `./emabook ~/code/haskell-knowledge-base` should just work.

To triage,

- [ ] Redirect to README.md if there is no index.md
- [ ] ... many more in my private notebook.

Before public release

- [ ] Finalize in HTML templating: heist vs a more popular one?
  - If using Heist, document how to add custom splices when using this project as a Haskell library.
- [ ] Finalize on the project name: `emabook`, or something else?

Documentation

- Heist docs for Ema
  - Helper.Heist
  - Helper.Heist.Tailwind - for `<Tailwind-Include />` in head that uses inline CSS in dev server, and include of generated CSS in prod.