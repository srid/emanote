{
  description = "emanote: Emanate a structured view of your plain-text notes";
  nixConfig = {
    extra-substituters = "https://cache.nixos.asia/oss";
    extra-trusted-public-keys = "oss:KO872wNJkCDgmGN3xy9dT89WAhvv13EiKncTtHDItVU=";
  };
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # Independent pin tracking unstable HEAD, used by tests/shell.nix
    # for playwright-driver. Decoupled from `nixpkgs` so the Haskell
    # build's pin and the Playwright pin update on different cadences.
    nixpkgs-latest.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";
    git-hooks.url = "github:bmrips/git-hooks.nix";
    git-hooks.flake = false;
    nixos-unified.url = "github:srid/nixos-unified";

    # These are not (necessarily) upstreamed to nixpkgs, yet.
    ema.url = "github:srid/ema";
    ema.flake = false;
    lvar.url = "github:srid/lvar/0.2.0.0";
    lvar.flake = false;
    heist-extra.url = "github:srid/heist-extra";
    heist-extra.flake = false;
    unionmount.url = "github:srid/unionmount/0.3.0.0";
    unionmount.flake = false;
    commonmark-simple.url = "github:srid/commonmark-simple/fix/flatten-nested-links";
    commonmark-simple.flake = false;
    commonmark-wikilink.url = "github:srid/commonmark-wikilink/master";
    commonmark-wikilink.flake = false;

    emanote-template.url = "github:srid/emanote-template";
    emanote-template.flake = false;

    # dpella/mcp is newer on GitHub than in nixpkgs' all-cabal-hashes,
    # so pin the source directly. mcp-server/ and mcp-types/ are subdirs.
    dpella-mcp.url = "github:dpella/mcp/52d13472d23ec11b9f6109f0fbf5159e9fda93da";
    dpella-mcp.flake = false;
    dpella-jsonrpc.url = "github:dpella/jsonrpc/0a708eb6c2744e1d69822d1cb90e9e352455a51b";
    dpella-jsonrpc.flake = false;
  };
  outputs = inputs:
    inputs.nixos-unified.lib.mkFlake { inherit inputs; root = ./.; };
}
