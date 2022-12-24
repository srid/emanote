---
page: 
  headHtml: |
    <snippet var="js.highlightjs" />
    <with var="js">
    <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/${value:highlightjs-ver}/languages/vim.min.js"></script>
    </with>
---

# Vim

[Vim](https://www.vim.org/), and its very popular fork
[Neovim](https://neovim.io/), are [modal text editors](https://unix.stackexchange.com/a/57708)
for editing plain text files. You can use it to edit [[markdown|Markdown]] files.

NOTE: If you are looking to write a Vim extension for Emanote, see [[export]].

## Known issue: glitches when saving file

Vim editors has a feature that clashes with Emanote and can cause slight glitchy
hickups when saving a file, while producing logs such as:

```text
[Info#emanote] [15:52:41] Re-registering file: ./docs/4913 R[/4913]
[Info#emanote] [15:52:41] Removing note: guide.md
[Info#emanote] [15:52:41] Reading file: ./docs/guide.md
[Info#emanote] [15:52:47] Re-registering file: ./docs/4913 R[/4913]
[Info#emanote] [15:52:47] Removing note: guide.md
[Info#emanote] [15:52:47] Reading file: ./docs/guide.md
[Info#emanote] [15:52:50] Re-registering file: ./docs/4913 R[/4913]
[Info#emanote] [15:52:50] Removing note: guide.md
[Info#emanote] [15:52:50] Reading file: ./docs/guide.md
```

Compared to the much slimmer logs from when using for example [[vscode]]:

```text
[Info#emanote] [15:54:39] Reading file: ./docs/guide.md
[Info#emanote] [15:54:41] Reading file: ./docs/guide.md
[Info#emanote] [15:54:43] Reading file: ./docs/guide.md
```

Temporary workaround (no longer applies after restarting Vim):

```vim
:set backupcopy=yes
```

Permanent workaround by adding this to your (Neo)vim config file, e.g `~/.vimrc`
for Vim or `~/.config/nvim/init.vim` for Neovim (see `:help vimrc` for more info):

```vim
" Must be added AFTER `set nocompatible`, if any, as `set nocompatible`
" resets the `backupcopy` setting.
set backupcopy=yes
```

Background: Vim has this great feature called "backup copies" that ensure that
your text isn't lost in the case of a faulty write/save operation. To achieve
this it uses some additional files, usually one with a 4-digit long integer and
one with the same name but with a tilde at the end (e.g `myfile.md` vs
`myfile.md~`), that it does this little dance with these files before discarding
them when the write/save operation succeeds.

For more info about this feature, see
[`:help backup`](https://vimhelp.org/editing.txt.html#backup),
[`:help backupcopy`](https://vimhelp.org/options.txt.html#%27backupcopy%27),
and [`:help writebackup`](https://vimhelp.org/options.txt.html#%27writebackup%27).

This also affects good 'ol [vi](http://ex-vi.sourceforge.net/).

Reference discussion: <https://github.com/srid/emanote/issues/180#issuecomment-945049455>
