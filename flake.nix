# ZERO flake inputs, by design.
#
# nixpkgs and the rest are pinned by npins (`npins/sources.json`) and
# loaded through `nix/nixpkgs.nix`. Flake inputs are costly to resolve on
# `nix develop` eval; zero inputs keeps it snappy, and the same pins are
# also usable from `nix-build`/`nix-shell` (default.nix, shell.nix).
{
  description = "emanote: Emanate a structured view of your plain-text notes";
  nixConfig = {
    extra-substituters = "https://cache.nixos.asia/oss";
    extra-trusted-public-keys = "oss:KO872wNJkCDgmGN3xy9dT89WAhvv13EiKncTtHDItVU=";
  };

  outputs = { self }:
    let
      sources = import ./npins;
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
      forEachSystem = f: builtins.listToAttrs (map
        (system: {
          name = system;
          value = f (import ./nix/nixpkgs.nix { inherit system; });
        })
        systems);
    in
    {
      homeManagerModules.default = import ./nix/home/emanote.nix;
      homeManagerModule = self.homeManagerModules.default; # legacy alias

      flakeModule = ./nix/flake-module;

      templates.default = {
        description = "A simple flake.nix template for emanote notebooks";
        path = builtins.path {
          path = sources.emanote-template;
          filter = path: _: baseNameOf path == "flake.nix";
        };
      };

      packages = forEachSystem (pkgs:
        let outs = import ./default.nix { inherit pkgs; };
        in {
          default = outs.default;
          emanote = outs.emanote;
          docs = outs.docs;
        });

      apps = forEachSystem (pkgs:
        let outs = import ./default.nix { inherit pkgs; };
        in {
          default = {
            type = "app";
            program = pkgs.lib.getExe outs.emanote;
            meta.description = "Emanote live server";
          };
          docs = outs.docs-app;
        });

      devShells = forEachSystem (pkgs: {
        default = import ./shell.nix { inherit pkgs; };
      });

      checks = forEachSystem (pkgs:
        let outs = import ./default.nix { inherit pkgs; };
        in {
          closure-size = outs.closure-size;
        } // pkgs.lib.optionalAttrs (outs.docs-linkCheck != null) {
          docs-linkCheck = outs.docs-linkCheck;
        });
    };
}
