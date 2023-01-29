{
  description = "emanote: Emanate a structured view of your plain-text notes";
  nixConfig = {
    extra-substituters = "https://srid.cachix.org";
    extra-trusted-public-keys = "srid.cachix.org-1:3clnql5gjbJNEvhA/WQp7nrZlBptwpXnUk6JAv8aB2M=";
  };
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    flake-root.url = "github:srid/flake-root";
    check-flake.url = "github:srid/check-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.check-flake.flakeModule
        inputs.flake-root.flakeModule
        inputs.treefmt-nix.flakeModule
        ./nix/emanote.nix
        ./nix/docker.nix
        ./nix/stork.nix
        ./nix/tailwind.nix
      ];
      perSystem = { pkgs, lib, config, ... }: {

        # haskell-flake configuration
        haskellProjects.default = {
          packages.emanote.root = ./.;
          buildTools = hp:
            let
              # Workaround for https://github.com/NixOS/nixpkgs/issues/140774
              fixCyclicReference = drv:
                pkgs.haskell.lib.overrideCabal drv (_: {
                  enableSeparateBinOutput = false;
                });
            in
            {
              inherit (config.packages)
                stork;
              treefmt = config.treefmt.build.wrapper;
              ghcid = fixCyclicReference hp.ghcid;
              haskell-language-server = hp.haskell-language-server.overrideScope (lself: lsuper: {
                ormolu = fixCyclicReference hp.ormolu;
              });
            } // config.treefmt.build.programs;
          source-overrides = {
            #inherit (inputs)
            #  heist-extra heist;
            # ema = inputs.ema + /ema;
          };
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
              #heist = dontCheck super.heist; # Tests are broken.
              #tailwind = addBuildDepends (unmarkBroken super.tailwind) [ config.packages.tailwind ];
              #commonmark-extensions = self.callHackage "commonmark-extensions" "0.2.3.2" { };
              commonmark-extensions = super.commonmark-extensions_0_2_3_2;
              emanote =
                lib.pipe super.emanote [
                  (lib.flip addBuildDepends [ config.packages.stork ])
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
        flakeModule = import ./nix/emanote.nix;
        # CI configuration
        herculesCI.ciSystems = [ "x86_64-linux" "aarch64-darwin" ];
      };
    };
}
