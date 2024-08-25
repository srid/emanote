# emanote

[![AGPL](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://en.wikipedia.org/wiki/Affero_General_Public_License)
[![built with nix](https://img.shields.io/badge/Built_With-Nix-5277C3.svg?logo=nixos&labelColor=73C3D5)](https://builtwithnix.org)
[![Matrix](https://img.shields.io/matrix/ema:matrix.org)](https://app.element.io/#/room/#ema:matrix.org "Chat on Matrix")
[![Harmeless Code of Conduct](https://img.shields.io/badge/harmless-8A2BE2)](https://srid.ca/coc "This project follows the 'Harmlessness Code of Conduct'")

Emanote emanates[^def] a structured view of your plain-text notes.

[^def]: **emanate**: *(of something abstract but perceptible) issue or spread out from (a source)*

Create beautiful websites -- such as personal webpage, blog, wiki, Zettelkasten, notebook, knowledge-base, documentation, etc. from future-proof plain-text notes and arbitrary data -- with live preview that updates in real-time.

Emanote is spiritual successor to [neuron](https://neuron.zettel.page) based on [Ema](https://ema.srid.ca).

## Installing and using

https://emanote.srid.ca/start/install

### Examples

https://emanote.srid.ca/examples

## Developing

Emanote is a Haskell software.[^licenses] Thanks to Nix, this repository is pre-configured to provide a delightful development experience with full IDE support in Visual Studio Code.

See https://srid.ca/haskell-template/start for complete instructions, but briefly: [Install Nix](https://nixos.asia/en/install) and run `nix develop -c just run`.

See [architecture](https://emanote.srid.ca/architecture) for a high-level overview of the codebase.

### PR contribution guidelines

You may want to run `nix run github:juspay/omnix ci run` before opening a PR, but the CI will run it as well.

## Discussion

To discuss the emanote project, [join Matrix][matrix] or post in [GitHub Discussions][ghdiscuss].

[matrix]: https://matrix.to/#/#ema:matrix.org
[ghdiscuss]: https://github.com/srid/emanote/discussions

[^licenses]: Emanote uses software and resources that are licensed differently, viz.:
    - [Logo](https://www.svgrepo.com/svg/267765/paper-plane)
    - [Stork search](https://github.com/jameslittle230/stork/blob/master/license.txt)
    - [Tailwind CSS](https://github.com/tailwindlabs/tailwindcss/blob/master/LICENSE)
    - Various SVG icons are from [Heroicons](https://github.com/tailwindlabs/heroicons/blob/master/LICENSE)
