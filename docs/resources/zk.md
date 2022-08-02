---
page:
  headHtml: |
    <snippet var="js.highlightjs" />
    <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.5.1/languages/ini.min.js"></script>
    <snippet var="js.stork-search" />
---

# zk

[`zk`](https://github.com/mickael-menu/zk) is a command-line tool for working
with Zettelkasten projects. It focuses on being easy for automation and is
generic in the sense that it does not lock in to only supporting a certain note
taking tool, which makes it useful for [Obsidian](https://obsidian.md/),
[Neuron](https://neuron.zettel.page/), and Emanote notes collections.

## Configuring repo

Inside your notes root folder (such as your repository root if you're using Git),
run the `zk init` command:

```sh
zk init --no-input
```

Edit your newly created config file at `.zk/config.toml` and change it to the
following:

```toml
# .zk/config.toml

[note]
default-title = "untitled"
filename = "{{title}}"
extension = "md"
template = "default.md"

# If using https://github.com/srid/emanote-template then you can set the
# following to ignore everything except what's inside the "content" dir,
# such as the root README.md file:
#ignore = [
#	"!content/"
#]

[format.markdown]

# zk has a "wiki" link mode, but it includes the full paths.
# Emanote only needs the filename.
link-format = "[[{{filename}}]]"

[lsp.diagnostics]
# Each diagnostic can have for value: none, hint, info, warning, error

# Report titles of wiki-links as hints.
wiki-title = "hint"
# Warn for dead links between notes.
dead-link = "error"
```

There are a lot more configs available. Above only focuses on the minimal for
configs related to Emanote integration. Full documentation of the `zk` config
file: <https://github.com/mickael-menu/zk/blob/main/docs/config.md>

Such as the editor and FZF settings:

```toml
[tool]

# Default editor used to open notes. When not set, the EDITOR or VISUAL
# environment variables are used.
editor = "code --wait"
#editor = "nvim"

# Syntax highlighting when in commands such as "zk edit -i"
fzf-preview = "bat -p --color always {-1}"
```

## Editor integration

`zk` provides a LSP (Language Server Protocol) server to allow auto-completion
when writing `[[wiki links]]` and `#tags`.

Available plugins:

- [`zk-nvim`](https://github.com/mickael-menu/zk-nvim) for [[vim|Neovim]] 0.5+
- [`zk-vscode`](https://github.com/mickael-menu/zk-vscode) for [[vscode]]

If you're already using an editor plugin such as `coc.nvim`, or otherwise have
a different editor that already supports LSP (such as SublimeText) then
checkout `zk`'s [Editor LSP configurations](https://github.com/mickael-menu/zk/blob/main/docs/editors-integration.md#editor-lsp-configurations)
