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

    ema.url = "github:srid/ema/no-ws"; # https://github.com/srid/ema/pull/161
    ema.inputs.nixpkgs.follows = "nixpkgs";
    ema.inputs.haskell-flake.follows = "haskell-flake";
    ema.inputs.flake-parts.follows = "flake-parts";
    ema.inputs.treefmt-nix.follows = "treefmt-nix";
    ema.inputs.flake-root.follows = "flake-root";

    # Remove pin when https://github.com/srid/heist-extra/pull/6 is merged
    heist-extra.url = "github:srid/heist-extra/7d70a20c87fd2c1496148fe1169a2852d0410fff";
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
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.flake-root.flakeModule
        inputs.treefmt-nix.flakeModule
        ./nix/flake-module.nix
        ./nix/docker.nix
      ];

      perSystem = { pkgs, lib, config, system, ... }: {
        _module.args = import inputs.nixpkgs {
          inherit system;
          overlays = [
            (self: super: {
              # Stork is marked as broken on intel mac, but it does work.
              # Unfortunately we cannot test this code PATH due to lack of CI for intel mac (#335).
              stork = if system == "x86_64-darwin" then super.stork.overrideAttrs (_oa: { meta.broken = false; }) else super.stork;
            })
          ];
        };

        # haskell-flake configuration
        haskellProjects.default = {
          projectFlakeName = "emanote";
          imports = [
            inputs.ema.haskellFlakeProjectModules.output
          ];
          devShell.tools = hp: {
            inherit (pkgs)
              stork;
          };
          autoWire = [ "packages" "apps" "checks" ];

          packages = {
            unionmount.source = inputs.unionmount;
            commonmark-simple.source = inputs.commonmark-simple;
            commonmark-wikilink.source = inputs.commonmark-wikilink;
            fsnotify.source = "0.4.1.0"; # Not in nixpkgs, yet.
            ghcid.source = "0.8.8";
            heist-extra.source = inputs.heist-extra;
          };

          settings = {
            # Haskell packages in nixpkgs are often broken in many ways; ergo,
            # it is our responsibility to fix them here.
            fsnotify.check = false;
            heist.broken = false;
            ixset-typed.broken = false;
            ixset-typed.jailbreak = true;
            ema.jailbreak = true;
            pandoc-link-context.broken = false;
            pandoc-link-context.jailbreak = true;
            tagtree.broken = false;
            tagtree.jailbreak = true;
            tailwind.broken = false;
            tailwind.jailbreak = true;
            emanote = { name, pkgs, self, super, ... }: {
              imports = [
                ./nix/removeReferencesTo.nix
              ];
              check = false;
              extraBuildDepends = [ pkgs.stork ];
              separateBinOutput = false; # removeReferencesTo.nix doesn't work otherwise
              justStaticExecutables = true;
              removeReferencesTo = [
                self.pandoc
                self.pandoc_3_1_11
                self.pandoc-types
                self.warp
              ];
              custom = pkg: pkg.overrideAttrs (lib.addMetaAttrs {
                # https://github.com/NixOS/cabal2nix/issues/608
                longDescription = ''
                  Emanote is a tool for generating a structured view of your
                  plain-text notes on the web, as a statically generated
                  website as well as a local live server. 

                  For editing notes, you can use any text editor of your
                  choice including the likes of Obsidian.
                '';
              });
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
        devShells.default =
          lib.addMetaAttrs { description = "Emanote development environment"; }
            (pkgs.mkShell {
              name = "emanote-dev";
              inputsFrom = [
                config.haskellProjects.default.outputs.devShell
                config.treefmt.build.devShell
              ];
              packages = with pkgs; [
                just
              ];
            });

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
        templates.default = {
          description = "A simple flake.nix template for emanote notebooks";
          path = builtins.path { path = inputs.emanote-template; filter = path: _: baseNameOf path == "flake.nix"; };
        };
      };
    };
}
