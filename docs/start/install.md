---
order: 1
---

# Installing

## Official method

Emanote is supported on all popular operating systems through [Nix].

1. [Install Nix & enable Flakes](https://nixos.asia/en/install)
   1. For Windows, see [[wsl]] or [the Docker approach](https://github.com/srid/emanote/issues/230)
   1. For NixOS, see [[nix]]
1. Optional: Use the Nix binary cache: https://srid.cachix.org
2. Run `nix profile install github:srid/emanote` to install Emanote

>[!tip] Try before installing
>  You may also "try before installing" by running `nix run github:srid/emanote`. This still downloads emanote to your nix store (in `/nix/store`), but will not install it to the user profile like `nix profile install` does.

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

## Using the flake template

If you already have a directory containing your `.md` files, you can use the flake template to get started with Emanote.

### Default template

This template only initializes the directory with a `flake.nix` file, leaving you to configure the rest of the project yourself.

```bash
nix flake init -t github:srid/emanote
```

### Example template

This template initializes the directory with the following paths:
- `flake.nix`: The flake file
- `index.md`: The index page of your site
- `index.yaml`: The emanote configuration file
- `.github`: The GitHub Actions workflow for deploying your site to GitHub Pages
- `.vscode`: The recommended VSCode workspace settings and extensions


```bash
nix flake init -t github:srid/emanote#example
```
