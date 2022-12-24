{
  description = "emanote";
  nixConfig = {
    extra-substituters = "https://cache.srid.ca";
    extra-trusted-public-keys = "cache.srid.ca:8sQkbPrOIoXktIwI0OucQBXod2e9fDjjoEZWn8OXbdo=";
  };
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    # TODO: Dependencies waiting to go from Hackage to nixpkgs.
    heist-extra.url = "github:srid/heist-extra";
    heist-extra.flake = false;
    heist.url = "github:snapframework/heist"; # Waiting for 1.1.1.0 on nixpkgs cabal hashes
    heist.flake = false;
    ema.url = "github:srid/ema";
    ema.flake = false;
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
        ./nix/emanote.nix
        ./nix/docker.nix
        ./nix/stork.nix
        ./nix/tailwind.nix
      ];
      perSystem = { pkgs, lib, config, ... }: {
        haskellProjects.default = {
          packages.emanote.root = ./.;
          buildTools = hp: {
            inherit (pkgs)
              treefmt
              nixpkgs-fmt;
            inherit (hp)
              cabal-fmt
              ormolu;
            inherit (config.packages)
              stork;
          };
          source-overrides = {
            inherit (inputs)
              heist-extra heist;
            ema = inputs.ema + /ema;
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
              heist = dontCheck super.heist; # Tests are broken.
              tailwind = addBuildDepends (unmarkBroken super.tailwind) [ config.packages.tailwind ];
              commonmark-extensions = self.callHackage "commonmark-extensions" "0.2.3.2" { };
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
