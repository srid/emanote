---
slug: install
order: 1
---

# Installing

## Quick Start

1. Install Nix [using these instructions](https://nixos.asia/en/install).
2. Run `nix profile install github:srid/emanote`[^nixpkgs]

[^nixpkgs]: Avoid using the Emanote version from `nixpkgs` repository, as that is **out-of-date** and furthermore the author is [prohibited](https://srid.ca/nixos-mod) from updating it.

## Platform-specific guides

- **Windows users**: See [[wsl]] for Windows Subsystem for Linux setup
- **Advanced Nix users**: See [[nix]] for Home Manager, flake-parts, and other advanced options

## Testing your installation {#test}

Once installed, test Emanote with:

```bash
# Run live server
cd /path/to/notebook
emanote run

# Generate static files
mkdir /tmp/output
emanote -L /path/to/notebook gen /tmp/output
```

[Nix]: https://nixos.asia/en/nix
