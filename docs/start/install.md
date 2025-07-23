---
slug: install
order: 1
---

# Installing

## Official method

>[!info] Avoiding installation
> Emanote need not have to be installed if you are using the [[emanote-template]] [flake](https://nixos.asia/en/flakes) to manage your content.

Emanote is supported on all popular operating systems through [Nix]. If you are new to Nix, checkout [this tutorial](https://nixos.asia/en/nix-first).

1. [Install Nix & enable Flakes](https://nixos.asia/en/install)
   1. For Windows, see [[wsl]]
   1. For NixOS, see [[nix]]
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

## Containers

If you do not wish to use Nix, you may try Emanote through Docker or Podman; see [[container]].

[Nix]: https://nixos.asia/en/nix
