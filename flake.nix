{
  description = "emanote: Emanate a structured view of your plain-text notes";
  nixConfig = {
    extra-substituters = "https://cache.garnix.io";
    extra-trusted-public-keys = "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g=";
  };
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    flake-root.url = "github:srid/flake-root";
    nixos-unified.url = "github:srid/nixos-unified";

    ema.url = "github:srid/ema";
    ema.flake = false;
    lvar.url = "github:srid/lvar";
    lvar.flake = false;

    heist-extra.url = "github:srid/heist-extra";
    heist-extra.flake = false;

    unionmount.url = "github:srid/unionmount";
    unionmount.flake = false;

    commonmark-simple.url = "github:srid/commonmark-simple";
    commonmark-simple.flake = false;
    commonmark-wikilink.url = "github:srid/commonmark-wikilink";
    commonmark-wikilink.flake = false;

    emanote-template.url = "github:srid/emanote-template";
    emanote-template.flake = false;
  };
  outputs = inputs:
    inputs.nixos-unified.lib.mkFlake { inherit inputs; root = ./.; };
}
