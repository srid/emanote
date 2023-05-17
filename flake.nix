{
  description = "emanote: Emanate a structured view of your plain-text notes";
  nixConfig = {
    extra-substituters = "https://srid.cachix.org";
    extra-trusted-public-keys = "srid.cachix.org-1:3clnql5gjbJNEvhA/WQp7nrZlBptwpXnUk6JAv8aB2M=";
  };
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "path:/Users/srid/code/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    flake-root.url = "github:srid/flake-root";
    check-flake.url = "github:srid/check-flake";

    ema.url = "github:srid/ema";
    ema.inputs.nixpkgs.follows = "nixpkgs";
    ema.inputs.haskell-flake.follows = "haskell-flake";
    ema.inputs.flake-parts.follows = "flake-parts";
    ema.inputs.check-flake.follows = "check-flake";
    ema.inputs.treefmt-nix.follows = "treefmt-nix";
    ema.inputs.flake-root.follows = "flake-root";

    cachix-push.url = "github:juspay/cachix-push";
  };
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.check-flake.flakeModule
        inputs.flake-root.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.cachix-push.flakeModule
        ./nix/flake-module.nix
        ./nix/docker.nix
        ./nix/stork.nix
      ];
      # Sensible package overrides for local packages.
      flake.haskellFlakeProjectModules.localDefaults = { pkgs, lib, config, ... }: {
        packages =
          let locals = config.defaults.packages;
          in lib.mapAttrs
            (_: v: {
              haddock = false; # Because, this is end-user software. No need for library docs.
              libraryProfiling = false; # Avoid double-compilation.
              justStaticExecutables = true; # Avoid needless runtime deps.
            })
            locals;
      };
      perSystem = { pkgs, lib, config, system, ... }: {
        cachix-push.cacheName = "srid";
        _module.args = import inputs.nixpkgs {
          inherit system;
          overlays = [
            (self: super: {
              stork-emanote = config.packages.stork;
            })
          ];
        };

        # haskell-flake configuration
        haskellProjects.default = {
          imports = [
            inputs.self.haskellFlakeProjectModules.localDefaults
            inputs.ema.haskellFlakeProjectModules.packages
          ];
          devShell.tools = hp: {
            inherit (config.packages)
              stork;
            treefmt = config.treefmt.build.wrapper;
          } // config.treefmt.build.programs;
          packages = {
            commonmark-extensions.root = "0.2.3.2";
            emanote = { pkgs, ... }: {
              extraBuildDepends = [ pkgs.stork-emanote ];
              removeReferencesTo = self: super: [
                self.pandoc
                self.pandoc-types
                self.warp
              ];
            };
          };
        };

        # treefmt-nix configuration
        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;

          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;

          # We use fourmolu
          programs.ormolu.package = pkgs.haskellPackages.fourmolu;
          settings.formatter.ormolu = {
            options = [
              "--ghc-opt"
              "-XImportQualifiedPost"
              "--ghc-opt"
              "-XTypeApplications"
            ];
          };
        };

        packages.default = config.packages.emanote;
        apps.default = config.apps.emanote;

        emanote = {
          package = config.packages.default;
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
        flakeModule = ./nix/flake-module.nix;
      };
    };
}
