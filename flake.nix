{
  description = "emanote";
  nixConfig = {
    extra-substituters = "https://cache.garnix.io";
    extra-trusted-public-keys = "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g=";
  };
  inputs = {
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";

    # Haskell dependency overrides
    ema.url = "github:srid/ema/multisite";
    ema.flake = false;
    pandoc-link-context.url = "github:srid/pandoc-link-context/master";
    pandoc-link-context.flake = false;
    heist-emanote.url = "github:srid/heist/emanote";
    heist-emanote.flake = false;
    ixset-typed.url = "github:well-typed/ixset-typed";
    ixset-typed.flake = false;
    tailwind-haskell.url = "github:srid/tailwind-haskell/master";
    tailwind-haskell.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
        ./flake-module.nix
      ];
      perSystem = { pkgs, system, inputs', self', ... }: {
        haskellProjects.default = {
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
              ema
              pandoc-link-context
              heist-emanote
              ixset-typed;
          };
          overrides = self: super: with pkgs.haskell.lib; {
            heist-emanote = dontCheck super.heist-emanote; # Tests are broken.
            inherit (inputs'.tailwind-haskell.packages)
              tailwind;
          };
        };
        emanote = {
          package = inputs.self.packages.${system}.default;
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
        homeManagerModule = import ./home-manager-module.nix;
        flakeModule = import ./flake-module.nix;
      };
    };
}
