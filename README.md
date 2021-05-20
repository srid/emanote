# emanote

WIP: Spiritual successor to [neuron](https://neuron.zettel.page), based on [Ema](https://ema.srid.ca).

Create beautiful websites -- such as personal webpage, blog, wiki, Zettelkasten, notebook, knowledge-base, documentation, etc. from future-proof plain-text notes and arbitrary data -- with live preview that updates in real-time.

Real-world example: [ema.srid.ca](https://ema.srid.ca) (generated from [these sources](https://github.com/srid/emanote/tree/master/docs)).

## Installing and using

```bash
# Run live server
PORT=8080 nix run github:srid/emanote -- -C /path/to/notebook

# Generate static files
mkdir /tmp/output
nix run github:srid/emanote -C /path/to/notebook gen /tmp/output
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

Then go back to Emanote, and edit its `.ghcid` file to refer to the haskell-knowledge-base directory instead. It should contain something like this:

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

Initial MVP,

- [x] Wiki-links
- Splice work
  - [x] Make sidebar tree a splice
  - [x] Make breadcrumbs a splice
    - Requires supporting arbitrary HTML in node children
  - [x] Make pandoc view a splice
- [x] Backlinks
  - Using ixset
- [x] Report error on web / CLI on markdown parse failure (generally on any error)
- [x] .emanote/templates/settings.yml - to pass global vars (`theme`, `site-title`) as-is
- [x] Use default templates and metadata if none exist
  - [x] Load templates from cabal data-files by default
  - [x] Do the same for `index.yaml` (then test on haskell-kb)
- [x] Use default static files (favicon.svg) for those that do not exist
- [x] Finish Pandoc AST rendering (address Unsupported)
- [x] Add docker image
- [x] Milestone: Make ema.srid.ca an emanote site
  - Bugs and blockers
    - [x] /start.md - the .md breaks links
    - [x] workaround raw html bug (see below) using video raw format
    - [x] "Next" styling, via class map in .yaml
  - [x] docs: adjust tutorial for new ema-template 
  - [x] ema-docs: replace with ema-template
- [x] Tailwind CDN: replace with windi workflow for faster page load, or use Twind shim
- [x] Avoid "Ema - Ema" kind of title. Pass ifIndexRoute splice?
- [x] BUG: /Haskell.org (with dot in it) crashes ema dev server
- [x] Milestone: `./emanote -C ~/code/haskell-knowledge-base` should just work.

Before tests (tasks impacting the larger architectural context in code base),
- [ ] UpTree.md?
  - ixset + path finding traversal
- [ ] Embedding: support `![[]]` of Obsidian? https://help.obsidian.md/How+to/Embed+files
  - Have `rewriteLinks` pass "title" to WikiLink parser, and have it return `WikiLink Video` (as distinct from `WikiLink Md`)
    - For embed flag, make that `WikiLink Embed Video` (vs `WikiLink (Conn Folge) Md`)
  - That, or do it from `<PandocLink>` style, in `rpBlock` by decoding "title" attr.
- [ ] Queries and results embed
- [ ] Finally, **tests**!
  - URL parsing (.md and wiki-links) and route encoding/decoding
  - Metadata overriding

To triage,
- [ ] Proper footnote styling: take Tufte style (sidebar refs) into consideration
- [ ] BUG: raw HTML doesn't work (eg: <video> element)
  - Blame https://github.com/snapframework/xmlhtml ?
    - Culprit, possibly: https://github.com/snapframework/xmlhtml/blob/54463f1691c7b31cc3c4c336a6fe328b1f0ebb95/src/Text/Blaze/Renderer/XmlHtml.hs#L27
- [ ] `emanote init` to allow editing default templates/yaml
- [ ] Add fsnotify watcher for default template files (etc), but only in ghcid mode
- [ ] Allow overriding baseUrl in CLI: `emanote gen --baseUrl=srid.github.io/foo`
- [x] Sidebar: expand-by-default on per-tree basis, by enabling it on yaml or frontmatter
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
