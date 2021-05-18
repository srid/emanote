# emabook

WIP: Spiritual successor to [neuron](https://neuron.zettel.page), based on [Ema](https://ema.srid.ca).


## Installing and using

```
# Install
nix-env -if https://github.com/srid/emabook/archive/refs/heads/master.tar.gz

# Run live server
PORT=8080 emabook -C /path/to/notebook

# Generate static files
mkdir /tmp/output
emabook -C /path/to/notebook gen /tmp/output
```

## Hacking

To develop with full IDE support in Visual Studio Code, follow these steps:

- [Install Nix](https://nixos.org/download.html) & [enable Flakes](https://nixos.wiki/wiki/Flakes)
- Run `nix-shell --run haskell-language-server` to sanity check your environment 
- [Open as single-folder workspace](https://code.visualstudio.com/docs/editor/workspaces#_singlefolder-workspaces) in Visual Studio Code
    - Install the [workspace recommended](https://code.visualstudio.com/docs/editor/extension-marketplace#_workspace-recommended-extensions) extensions
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
```

Then go back to Emabook, and edit its `.ghcid` file to refer to the haskell-knowledge-base directory instead. It should contain something like this:

```
--warnings -T ":main -C ../haskell-knowledge-base"
```

Finally, run `bin/run` to spin up the server, and go to http://localhost:9010/

To generate static files,

```bash
mkdir ./output
nix run . -- -C ../haskell-knowledge-base gen $(pwd)/output
nix-shell -p nodePackages.http-server --run 'http-server ./output/'
```

## Tasks

- [x] Wiki-links
- Splice work
  - [x] Make sidebar tree a splice
  - [x] Make breadcrumbs a splice
    - Requires supporting arbitrary HTML in node children
  - [x] Make pandoc view a splice
- [x] Backlinks
  - Using ixset
- [x] Report error on web / CLI on markdown parse failure (generally on any error)
- [x] .emabook/templates/settings.yml - to pass global vars (`theme`, `site-title`) as-is
- [x] Use default templates and metadata if none exist
  - [x] Load templates from cabal data-files by default
  - [x] Do the same for `index.yaml` (then test on haskell-kb)
- [ ] BUG: /Haskell.org (with dot in it) crashes ema dev server
- [ ] `emabook init` to allow editing default templates/yaml
- [ ] Redirect to README.md if there is no index.md (Obsidian publish behaviour)
- [ ] GitHub pages without CNAME: `emabook gen --base-url=srid.github.io/foo` (or some other way)
- [ ] Milestone: `./emabook ~/code/haskell-knowledge-base` should just work.

Before beta release,

- [ ] Finalize on the project name: `emabook`, or something else?

To triage,

- [ ] Sidebar: expand-by-default on per-tree basis, by enabling it on yaml or frontmatter
- [ ] Display directory contents
  - For every `${folder}.md` route, display its contents *in addition to* the actual content.
    - Control via YAML metadata.
- [ ] UpTree?
  - ixset + path finding traversal
- [ ] Heist Pandoc splice: allow custom "class library" with hierarchy:
  ```
  <Pandoc>
    <Custom>
      <Popout class="px-1 font-serif rounded bg-pink-50" />
      <!-- Hierarchical styling? -->
      <Warning class="px-1 rounded bg-gray-50">
        <Header>
          <h2>class="text-xl font-bold"</h2>
        </Header>
      </Warning>
    </Custom>
  </Pandoc>
  ```

Before public release

- [x] Finalize in HTML templating: heist vs a more popular one?
  - Probably gonna take the heist trade-off, given the ability to customize breadcrumbs/sidebar/pandoc HTML

Documentation

- Heist docs for Ema
  - Helper.Heist
  - Helper.Heist.Tailwind - for `<Tailwind-Include />` in head that uses inline CSS in dev server, and include of generated CSS in prod.
  - adding custom splices (when using as a library)
- Fsnotify limitations
  - If doing a directory move/rename, restart emabook.

Mega features,

- Powerful and simpler query system (cf. Obsidian search)
- Pandoc filters (`Pandoc -> IO Pandoc`)
  - Including citations
- mdBook like search (emabook should provide the index)
- Ref: [top requested neuron features](https://github.com/srid/neuron/issues?q=is%3Aissue+is%3Aopen+sort%3Areactions)