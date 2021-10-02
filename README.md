# emanote

[![AGPL](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://en.wikipedia.org/wiki/Affero_General_Public_License)
[![built with nix](https://img.shields.io/badge/Built_With-Nix-5277C3.svg?logo=nixos&labelColor=73C3D5)](https://builtwithnix.org)
[![FAIR](https://img.shields.io/badge/FAIR-pledge-blue)](https://www.fairforall.org/about/)
[![Matrix](https://img.shields.io/matrix/neuron:matrix.org)](https://app.element.io/#/room/#neuron:matrix.org "Chat on Matrix")
[![Liberapay](https://img.shields.io/liberapay/patrons/srid.svg?logo=liberapay)](https://liberapay.com/srid/donate "Donate using liberapay")

Spiritual successor to [neuron](https://neuron.zettel.page), based on [Ema](https://ema.srid.ca).

Create beautiful websites -- such as personal webpage, blog, wiki, Zettelkasten, notebook, knowledge-base, documentation, etc. from future-proof plain-text notes and arbitrary data -- with live preview that updates in real-time.

**Project Status**: In beta phase, but is [nearly 1.0](https://github.com/srid/emanote/milestone/3).

## Installing and using

```bash
# Install (`cachix use srid` for cached binaries)
nix-env -if https://github.com/srid/emanote/archive/refs/heads/master.tar.gz
# Or, from the Git repo: nix-env -if ./default.nix

# Run live server (HOST and PORT are optional)
cd /path/to/notebook
HOST=0.0.0.0 PORT=8001 emanote

# Generate static files (-L defaults to current directory)
mkdir /tmp/output
emanote -L /path/to/notebook gen /tmp/output
```

For other installation methods, see [here](https://note.ema.srid.ca/start/install).

### Examples

https://note.ema.srid.ca/examples

## Developing

Emanote is written in Haskell. Thanks to Nix, this repository is pre-configured to provide a delightful development experience with full IDE support in Visual Studio Code. Follow these steps:

- [Install Nix](https://nixos.org/download.html) & [enable Flakes](https://nixos.wiki/wiki/Flakes)
- Run `nix-shell --run haskell-language-server` to sanity check your environment (Expect it to download and build a bunch of things the first time)
- Open the repository [as single-folder workspace](https://code.visualstudio.com/docs/editor/workspaces#_singlefolder-workspaces) in Visual Studio Code
    - Install the [workspace recommended](https://code.visualstudio.com/docs/editor/extension-marketplace#_workspace-recommended-extensions) extensions
    - <kbd>Ctrl+Shift+P</kbd> to run command "Nix-Env: Select Environment" and select `shell.nix`. The extension will ask you to reload VSCode at the end.
- Press <kbd>Ctrl+Shift+B</kbd> in VSCode, or run `bin/run` (`bin/run-via-tmux` if you have tmux installed) in terminal, to launch the dev version Emanote on `./docs`, then navigate to http://localhost:9010/
  - Changing the Haskell sources will recompile and reload this instance automatically.

All but the final step need to be done only once. See [architecture.md](docs/architecture.md) for a high-level overview of the codebase.

## Discussion

To discuss the emanote project, [join Matrix][matrix] or post in [GitHub Discussions][ghdiscuss].

[matrix]: https://matrix.to/#/#neuron:matrix.org
[ghdiscuss]: https://github.com/srid/emanote/discussions
