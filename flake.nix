{
  description = "emanote";
  nixConfig = {
    extra-substituters = "https://cache.garnix.io";
    extra-trusted-public-keys = "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g=";
  };
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";

    # Haskell dependency overrides
    ema.url = "github:srid/ema/multisite";
    ema.flake = false;
    tailwind-haskell.url = "github:srid/tailwind-haskell/master";
    tailwind-haskell.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
        ./nix/emanote.nix
        ./nix/docker.nix
      ];
      perSystem = { pkgs, inputs', self', ... }: {
        haskellProjects = {
          default = {
            root = ./.;
            buildTools = hp: {
              inherit (pkgs)
                treefmt
                nixpkgs-fmt;
              inherit (hp)
                cabal-fmt
                ormolu;
              inherit (inputs'.tailwind-haskell.packages)
                tailwind;
            };
            source-overrides = {
              inherit (inputs)
                ema;
            };
            overrides = self: super: with pkgs.haskell.lib; {
              heist-emanote = dontCheck (doJailbreak (unmarkBroken super.heist-emanote)); # Tests are broken.
              ixset-typed = unmarkBroken super.ixset-typed;
              pandoc-link-context = unmarkBroken super.pandoc-link-context;
              inherit (inputs'.tailwind-haskell.packages)
                tailwind;
            };
          };
          ghc92 = {
            root = ./.;
            haskellPackages = pkgs.haskell.packages.ghc923;
            source-overrides = {
              inherit (inputs)
                ema;
            };
            overrides = self: super: with pkgs.haskell.lib; {
              heist-emanote = dontCheck (doJailbreak (unmarkBroken super.heist-emanote)); # Tests are broken.
              ixset-typed = unmarkBroken super.ixset-typed;
              pandoc-link-context = unmarkBroken super.pandoc-link-context;
              inherit (inputs'.tailwind-haskell.packages)
                tailwind;

              # Needed on GHC 9.2
              relude = dontCheck (self.callHackage "relude" "1.1.0.0" { });
              retry = dontCheck super.retry; # For GHC 9.2.
              streaming-commons = dontCheck super.streaming-commons; # Fails on darwin
              http2 = dontCheck super.http2; # Fails on darwin
              generic-data = dontCheck super.generic-data;
              type-errors-pretty = dontCheck (doJailbreak super.type-errors-pretty);
            };
          };
        };
        packages.test =
          pkgs.runCommand "emanote-test" { } ''
            ${pkgs.lib.getExe self'.packages.default} --test 2>&1 | tee $out
          '';
        emanote = {
          package = self'.packages.default;
          sites = {
            "docs" = {
              path = ./docs;
              pathString = "./docs";
              allowBrokenLinks = true; # A couple, by design, in demo.md
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
