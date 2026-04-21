{
  description = "emanote: Emanate a structured view of your plain-text notes";
  nixConfig = {
    extra-substituters = "https://cache.nixos.asia/oss";
    extra-trusted-public-keys = "oss:KO872wNJkCDgmGN3xy9dT89WAhvv13EiKncTtHDItVU=";
  };
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";
    git-hooks.url = "github:bmrips/git-hooks.nix";
    git-hooks.flake = false;
    nixos-unified.url = "github:srid/nixos-unified";

    # These are not (necessarily) upstreamed to nixpkgs, yet.
    ema.url = "github:srid/ema/0.12.0.0";
    ema.flake = false;
    lvar.url = "github:srid/lvar/0.2.0.0";
    lvar.flake = false;
    # Pinned to the exact commit on `footnote-id-prefix` rather than
    # the branch ref so that a future `nix flake update` cannot silently
    # pick up a force-push or rebase. Flip back to `master` / a released
    # tag once https://github.com/srid/heist-extra/pull/XX merges.
    heist-extra.url = "github:srid/heist-extra/387ecc1c8138c9af09d27503aed7fed76f88fd8b";
    heist-extra.flake = false;
    unionmount.url = "github:srid/unionmount/0.3.0.0";
    unionmount.flake = false;
    commonmark-simple.url = "github:srid/commonmark-simple/0.2.0.0";
    commonmark-simple.flake = false;
    commonmark-wikilink.url = "github:srid/commonmark-wikilink/0.2.0.0";
    commonmark-wikilink.flake = false;

    emanote-template.url = "github:srid/emanote-template";
    emanote-template.flake = false;
  };
  outputs = inputs:
    inputs.nixos-unified.lib.mkFlake { inherit inputs; root = ./.; };
}
