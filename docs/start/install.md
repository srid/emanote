---
order: 1
---

# Installing

## Official method

Emanote is supported on all popular operating systems through [Nix].

1. [Install Nix](https://nixos.org/download.html) & [enable Flakes](https://nixos.wiki/wiki/Flakes#Installing_flakes)
   1. For Windows, see [[wsl]] or [the Docker approach](https://github.com/srid/emanote/issues/230)
   1. For NixOS, see [[nix]]
1. Optional: [Use](https://nixos.wiki/wiki/Binary_Cache#Using_a_binary_cache) the Nix binary cache: 
     - https://cache.srid.ca (key: `cache.srid.ca:8sQkbPrOIoXktIwI0OucQBXod2e9fDjjoEZWn8OXbdo=`)
2. Run `nix profile install github:srid/emanote` to install Emanote[^try]

[^try]: You may also "try before installing" by running `nix run github:srid/emanote`. This still downloads emanote to your nix store, `/nix`, but will not install it to the user profile like `nix profile install` does.

To test your Emanote install,

```bash
# Run live server
cd /path/to/notebook
emanote run

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

Nix and [[docker]] are the only mechanisms the author intends to support in the long-term. Emanote is also available in nixpkgs.
