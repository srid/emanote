{
  description = "emanote: Emanate a structured view of your plain-text notes";
  nixConfig = {
    extra-substituters = "https://cache.nixos.asia/oss";
    extra-trusted-public-keys = "oss:KO872wNJkCDgmGN3xy9dT89WAhvv13EiKncTtHDItVU=";
  };

  # Keep this user-facing flake input-free. Nix verifies every flake input on
  # each invocation; the same revisions are pinned by npins and consumed through
  # the standalone libraries exposed by haskell-flake and the development tools.
  outputs = { self }:
    let
      sources = import ./npins;
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      eachSystem = f: builtins.listToAttrs (map
        (system: {
          name = system;
          value = f system;
        })
        systems);
      systemOutputs = eachSystem (system: import ./nix/system.nix {
        inherit sources system;
        root = ./.;
      });
      outputFor = name: builtins.mapAttrs (_: output: output.${name}) systemOutputs;
    in
    {
      packages = outputFor "packages";
      apps = outputFor "apps";
      devShells = outputFor "devShells";
      checks = outputFor "checks";

      homeManagerModule = { lib, pkgs, ... }: {
        imports = [ ./nix/modules/home/emanote.nix ];
        services.emanote.package = lib.mkDefault self.packages.${pkgs.stdenv.hostPlatform.system}.default;
      };
      flakeModule = ./nix/modules/flake-parts/flake-module;
      templates.default = {
        description = "A simple flake.nix template for emanote notebooks";
        path = builtins.path {
          name = "emanote-template";
          path = sources.emanote-template;
          filter = path: _: baseNameOf path == "flake.nix";
        };
      };

      # Retain the Omnix CI contract previously declared by the flake-parts
      # small-closure module. Its system list is intentionally empty today.
      om.ci.default.emanote = {
        dir = ".";
        steps.custom.closure-size = {
          type = "app";
          name = "check-closure-size";
          systems = [ ];
        };
      };
    };
}
