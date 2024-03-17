{
  description = "emanote: Emanate a structured view of your plain-text notes";
  nixConfig = {
    extra-substituters = "https://cache.garnix.io";
    extra-trusted-public-keys = "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g=";
  };
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    flake-root.url = "github:srid/flake-root";

    ema.url = "github:srid/ema";
    ema.inputs.nixpkgs.follows = "nixpkgs";
    ema.inputs.haskell-flake.follows = "haskell-flake";
    ema.inputs.flake-parts.follows = "flake-parts";
    ema.inputs.treefmt-nix.follows = "treefmt-nix";
    ema.inputs.flake-root.follows = "flake-root";

    heist-extra.url = "github:srid/heist-extra";
    heist-extra.flake = false;

    unionmount.url = "github:srid/unionmount";
    unionmount.flake = false;

    commonmark-simple.url = "github:srid/commonmark-simple";
    commonmark-simple.flake = false;
    commonmark-wikilink.url = "github:srid/commonmark-wikilink";
    commonmark-wikilink.flake = false;

    emanote-template.url = "github:srid/emanote-template";
    emanote-template.flake = false;

    haskell-template.url = "github:locallycompact/haskell-template";
  };
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.flake-root.flakeModule
        inputs.treefmt-nix.flakeModule
        ./nix/flake-module.nix
        (inputs.haskell-template.flakeModules.horizon-package-set)
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
            inputs.self.haskellFlakeProjectModules.horizon-package-set
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

            # Not in Horizon
            monad-logger-extras.source = "0.1.1.1";
            aeson-extra.source = "0.5.1.3";
            aeson-optics.source = "1.2.1";
            feed.source = "1.3.2.1";
            xmlhtml.source = "0.2.5.4";
            map-syntax.source = "0.3";
            pandoc-lua-engine.source = "0.2.1";
            hslua-repl.source = "0.1.1";
            hslua-module-zip.source = "1.1.0";
            isocline.source = "1.0.9";
            doctest-driver-gen.source = "0.3.0.8";
            heist.source = "1.1.1.2";
            ixset-typed.source = "0.5.1.0";
            safecopy.source = "0.10.4.2";
            lens-action.source = "0.2.6";
            pandoc-link-context.source = "1.4.1.0";
            tagtree.source = "0.1.0.1";
            tailwind.source = "0.3.0.0";
            dir-traverse.source = "0.2.3.0";
            dirstream.source = "1.1.0";
            hinotify.source = "0.4.1";
          };

          settings = {
            # Horizon stuff
            doctest-driver-gen.check = false;
            aeson-extra.check = false;

            # Haskell packages in nixpkgs are often broken in many ways; ergo,
            # it is our responsibility to fix them here.
            fsnotify.check = false;
            heist.broken = false;
            heist.check = false;
            ixset-typed.broken = false;
            ixset-typed.jailbreak = true;
            pandoc-link-context.broken = false;
            pandoc-link-context.jailbreak = true;
            tailwind.broken = false;
            tailwind.jailbreak = true;
            unionmount.check = !pkgs.stdenv.isDarwin; # garnix: Slow M1 builder 
            emanote = { name, pkgs, self, super, ... }: {
              check = false;
              extraBuildDepends = [ pkgs.stork ];
              separateBinOutput = false; # removeReferencesTo.nix doesn't work otherwise
              justStaticExecutables = true;
              removeReferencesTo = [
                self.pandoc
                #self.pandoc_3_1_11
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

        apps.check-closure-size.program = pkgs.writeShellApplication {
          name = "emanote-check-closure-size";
          runtimeInputs = [ pkgs.jq pkgs.bc pkgs.nix ];
          meta.description = "Check that emanote's nix closure size remains reasonably small";
          text = ''
            MAX_CLOSURE_SIZE=$(echo "600 * 1000000" | bc)
            CLOSURE_SIZE=$(nix path-info --json -S .#default | jq '.[0]'.closureSize)
            echo "Emanote closure size: $CLOSURE_SIZE"
            echo "    Max closure size: $MAX_CLOSURE_SIZE"
            if [ "$CLOSURE_SIZE" -gt "$MAX_CLOSURE_SIZE" ]; then
                echo "ERROR: Emanote's nix closure size has increased"
                exit 3
            else
                echo "OK: Emanote's nix closure size is within limits"
            fi
          '';
        };

        emanote = {
          package = config.packages.default;
          sites = {
            "docs" = {
              layers = [{ path = ./docs; pathString = "./docs"; }];
              allowBrokenLinks = true; # A couple, by design, in markdown.md
              prettyUrls = true;
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
