---
order: 1
---

# Installing

## Official method

Emanote is supported on all popular operating systems through [Nix].

1. Install [Nix] (for Windows, see [[wsl]])
2. Run `nix-env -if https://github.com/srid/emanote/archive/refs/heads/master.tar.gz` to install Emanote

To test your Emanote install,

```sh
# Run live server (HOST and PORT are optional)
cd /path/to/notebook
HOST=0.0.0.0 PORT=8001 emanote

# Generate static files (-L defaults to current directory)
mkdir /tmp/output
emanote -L /path/to/notebook gen /tmp/output
```

[Nix]: https://nixos.org/download.html

## Non-nix methods

If you do not wish to use Nix, you may use Emanote in one of the following ways:

| Method     | Long-term support | Platforms         |
| ---------- | ----------------- | ----------------- |
| [[docker]] | Guaranteed        | Linux, WSL, macOS |

Emanote is not on [Hackage](https://hackage.haskell.org/) yet, as 1.0 is yet to be released. Once on Hackage, Emanote can be packaged up in various ways like [Pandoc](https://pandoc.org/) (another Haskell software on Hackage) is. It will become the distributor's responsibility to create and maintain this long-term. Nix and [[docker]] are the only mechanisms the author intends to support in the long-term. 
