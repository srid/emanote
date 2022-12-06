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

    # TODO: Dependencies waiting to go from Hackage to nixpkgs.
    heist-extra.url = "github:srid/heist-extra";
    heist-extra.flake = false;
    heist.url = "github:snapframework/heist"; # Waiting for 1.1.1.0 on nixpkgs cabal hashes
    heist.flake = false;
    ema.url = "github:EmaApps/ema";
    ema.flake = false;
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
          overrides = self: super: with pkgs.haskell.lib; {
            heist = dontCheck super.heist; # Tests are broken.
            tailwind = addBuildDepends (unmarkBroken super.tailwind) [ config.packages.tailwind ];
            commonmark-extensions = self.callHackage "commonmark-extensions" "0.2.3.2" { };
            emanote = addBuildDepends super.emanote [ config.packages.stork ];
          };
        };
        packages.default =
          let
            haskellExeSansDependencyBloat = pkg: get-deps: with pkgs.haskell.lib;
              (justStaticExecutables pkg).overrideAttrs (old: rec {
                disallowedReferences = get-deps config.haskellProjects.default.haskellPackages;
                # Ditch data dependencies that are not needed at runtime.
                # cf. https://github.com/NixOS/nixpkgs/pull/204675
                postInstall = (old.postInstall or "") + ''
                  ${lib.concatStrings (map (e: "echo Removing reference to: ${e}\n") disallowedReferences)}
                  ${lib.concatStrings (map (e: "remove-references-to -t ${e} $out/bin/emanote\n") disallowedReferences)}
                '';
              });
          in
          haskellExeSansDependencyBloat config.packages.emanote
            (hp: with hp; [
              pandoc
              pandoc-types
              warp
            ]);
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
      };
    };
}
