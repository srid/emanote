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
    haskell-flake.url = "github:srid/haskell-flake";
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
  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
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
      perSystem = { pkgs, lib, config, ... }: {
        cachix-push.cacheName = "srid";

        # haskell-flake configuration
        haskellProjects.default = {
          imports = [
            inputs.ema.haskellFlakeProjectModules.output
          ];
          devShell.tools = hp: {
            inherit (config.packages)
              stork;
            treefmt = config.treefmt.build.wrapper;
          } // config.treefmt.build.programs;
          overrides = with pkgs.haskell.lib;
            let
              # Remove the given references from drv's executables.
              # We shouldn't need this after https://github.com/haskell/cabal/pull/8534
              removeReferencesTo = disallowedReferences: drv:
                drv.overrideAttrs (old: rec {
                  inherit disallowedReferences;
                  # Ditch data dependencies that are not needed at runtime.
                  # cf. https://github.com/NixOS/nixpkgs/pull/204675
                  # cf. https://srid.ca/remove-references-to
                  postInstall = (old.postInstall or "") + ''
                    ${lib.concatStrings (map (e: "echo Removing reference to: ${e}\n") disallowedReferences)}
                    ${lib.concatStrings (map (e: "remove-references-to -t ${e} $out/bin/*\n") disallowedReferences)}
                  '';
                });
            in
            self: super: {
              commonmark-extensions = super.commonmark-extensions_0_2_3_2;
              emanote =
                lib.pipe super.emanote [
                  (lib.flip addBuildDepends [ config.packages.stork ])
                  dontHaddock
                  justStaticExecutables
                  (removeReferencesTo [
                    self.pandoc
                    self.pandoc-types
                    self.warp
                  ])
                ];
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
