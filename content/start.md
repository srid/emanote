---
order: 1
---

# Getting Started

As first steps, perform the following before proceeding to the tutorial section below:

1. [Install Nix](https://nixos.org/download.html) (see [platform-specific notes here](https://neuron.zettel.page/install))
1. [Enable Flakes](https://nixos.wiki/wiki/Flakes#Installing_flakes)
1. Clone [the template repository](https://github.com/srid/ema-docs) locally
1. Run `bin/run` and access the site at <http://localhost:9001>

That should start the Ema dev server displaying a replica of ema.srid.ca. Go ahead and try modifying either the Markdown content in `./content` or the Haskell source in `./src/Main.hs`, and observe how the web view updates [instantly](concepts/hot-reload.md).

{.last}
[Next]{.next}, [in the tutorial](start/tutorial.md) let's try using this template repo to create a basic website.
