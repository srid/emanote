# emanote

[![AGPL](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://en.wikipedia.org/wiki/Affero_General_Public_License)
[![built with nix](https://img.shields.io/badge/Built_With-Nix-5277C3.svg?logo=nixos&labelColor=73C3D5)](https://builtwithnix.org)
[![Matrix](https://img.shields.io/matrix/ema:matrix.org)](https://app.element.io/#/room/#ema:matrix.org "Chat on Matrix")

Spiritual successor to [neuron](https://neuron.zettel.page), based on [Ema](https://ema.srid.ca).

Create beautiful websites -- such as personal webpage, blog, wiki, Zettelkasten, notebook, knowledge-base, documentation, etc. from future-proof plain-text notes and arbitrary data -- with live preview that updates in real-time.

## Installing and using

https://emanote.srid.ca/start/install

### Examples

https://emanote.srid.ca/examples

## Developing

Emanote is written in Haskell. Thanks to Nix, this repository is pre-configured to provide a delightful development experience with full IDE support in Visual Studio Code. Follow these steps:

- [Install Nix](https://nixos.org/download.html) & [enable Flakes](https://nixos.wiki/wiki/Flakes#Installing_flakes)
- Run `nix develop -c haskell-language-server` to sanity check your environment (Expect it to download and build a bunch of things the first time)
- Open the repository [as single-folder workspace](https://code.visualstudio.com/docs/editor/workspaces#_singlefolder-workspaces) in Visual Studio Code
    - Install the [workspace recommended](https://code.visualstudio.com/docs/editor/extension-marketplace#_workspace-recommended-extensions) extensions
    - <kbd>Ctrl+Shift+P</kbd> to run command "Nix-Env: Select Environment" and select `shell.nix`. The extension will ask you to reload VSCode at the end.
- Press <kbd>Ctrl+Shift+B</kbd> in VSCode, or run `bin/run` in terminal, to launch the dev version Emanote on `./docs`, then navigate to http://localhost:9010/
  - Changing the Haskell sources will recompile and reload this instance automatically.

All but the final step need to be done only once. See [architecture.md](docs/architecture.md) for a high-level overview of the codebase.

## Discussion

To discuss the emanote project, [join Matrix][matrix] or post in [GitHub Discussions][ghdiscuss].

[matrix]: https://matrix.to/#/#ema:matrix.org
[ghdiscuss]: https://github.com/srid/emanote/discussions
