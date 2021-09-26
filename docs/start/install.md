---
order: 1
---

# Installing

## Official method

Follow the [README on GitHub](https://github.com/srid/emanote#emanote) to install Emanote using [Nix](https://nixos.org/download.html). This is the recommended approach. 

For Windows, see [[wsl]].

## Non-nix methods

If you do not wish to use Nix, you may use Emanote in one of the following ways:

| Method     | Long-term support | Platforms         |
| ---------- | ----------------- | ----------------- |
| [[docker]] | Guaranteed        | Linux, WSL, macOS |

Emanote is not on [Hackage](https://hackage.haskell.org/) yet, as 1.0 is yet to be released. Once on Hackage, Emanote can be packaged up in various ways like [Pandoc](https://pandoc.org/) (another Haskell software on Hackage) is. It will become the distributor's responsibility to create and maintain this long-term. Nix and [[docker]] are the only mechanisms the author intends to support in the long-term. 
