---
name: nix-haskell
description: Use this when working on a Haskell project with Nix. Covers haskell-flake setup, adding/overriding dependencies, package settings, and devShell configuration.
---

# Haskell + Nix with haskell-flake

Use [haskell-flake](https://github.com/srid/haskell-flake) — a flake-parts module. Start from [haskell-template](https://github.com/srid/haskell-template). Requires a `.cabal` or `cabal.project` file; haskell-flake scans for it automatically.

## Starting a new project

**You MUST use `git clone https://github.com/srid/haskell-template <project-dir>` to start a new project.** Do NOT fetch individual files, do NOT use `om init`, do NOT write flake.nix from scratch. Clone the template, then rename the package in `.cabal` and `nix/modules/flake/haskell.nix`.

## Project structure

Projects based on haskell-template use [nixos-unified](https://nixos-unified.org) autowiring. **Do NOT write Nix configuration inline in `flake.nix`.** The `flake.nix` delegates entirely to `nixos-unified`, and all Nix configuration goes in **`./nix/modules/flake/*.nix`** files.

## Docs

- haskell-flake guide: https://community.flake.parts/haskell-flake
- haskell-flake options: https://flake.parts/options/haskell-flake
- haskell-template: https://github.com/srid/haskell-template
