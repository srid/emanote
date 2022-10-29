{
  description = "emanote";
  nixConfig = {
    extra-substituters = "https://cache.garnix.io";
    extra-trusted-public-keys = "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g=";
  };
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
        ./nix/emanote.nix
        ./nix/docker.nix
        ./nix/stork.nix
        ./nix/tailwind.nix
      ];
      perSystem = { pkgs, inputs', self', ... }: {
        haskellProjects.default = {
          root = ./.;
          name = "emanote";
          buildTools = hp: {
            inherit (pkgs)
              treefmt
              nixpkgs-fmt;
            inherit (hp)
              cabal-fmt
              ormolu;
            inherit (self'.packages)
              stork;
          };
          overrides = self: super: with pkgs.haskell.lib; {
            tailwind = addBuildDepends (unmarkBroken super.tailwind) [ self'.packages.tailwind ];
            heist-emanote = dontCheck (doJailbreak (unmarkBroken super.heist-emanote)); # Tests are broken.
            ixset-typed = unmarkBroken super.ixset-typed;
            pandoc-link-context = unmarkBroken super.pandoc-link-context;
          };
          modifier = drv: with pkgs.haskell.lib;
            addBuildDepends drv [
              self'.packages.stork
            ];
        };
        packages.test =
          pkgs.runCommand "emanote-test" { } ''
            ${pkgs.lib.getExe self'.packages.default} --test 2>&1 | tee $out
          '';
        emanote = {
          package = self'.packages.default;
          sites = {
            "docs" = {
              layers = [ ./docs ];
              layersString = [ "./docs" ];
              allowBrokenLinks = true; # A couple, by design, in markdown.md
            };
          };
        };
      };
      flake = {
        homeManagerModule = import ./nix/home-manager-module.nix;
        flakeModule = import ./nix/emanote.nix;
      };
    };
}
