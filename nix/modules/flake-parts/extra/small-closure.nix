# To make sure that Emanote's closure size is small.
{ root, inputs, ... }:
let
  allowedSystems = [
    "x86_64-linux"

    # Disabling on macOS due to upstream bug:
    # - https://github.com/NixOS/nixpkgs/issues/318013
    # - https://github.com/NixOS/nixpkgs/pull/304352
    # - Related: https://github.com/NixOS/nix/pull/10877
    # "aarch64-darwin"
  ];
in
{
  perSystem = { pkgs, lib, config, system, ... }: lib.mkIf (lib.elem system allowedSystems) {
    haskellProjects.default = {
      settings = {
        emanote = { name, pkgs, self, super, ... }: {
          separateBinOutput = false; # removeReferencesTo.nix doesn't work otherwise
          justStaticExecutables = true;
          removeReferencesTo = [
            self.ghc
            self.pandoc
            self.pandoc-types
            self.warp
          ];
        };
      };
    };

    apps.check-closure-size = rec {
      inherit (program) meta;
      program = pkgs.writeShellApplication {
        name = "emanote-check-closure-size";
        runtimeInputs = [ pkgs.jq pkgs.bc pkgs.nix ];
        meta.description = "Check that emanote's nix closure size remains reasonably small";
        text = ''
          MAX_CLOSURE_SIZE=$(echo "600 * 1000000" | bc)
          CLOSURE_SIZE=$(nix path-info --json -S .#default | jq 'first(.[])'.closureSize)
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
    };
  };

  flake.om.ci.default.emanote = {
    dir = ".";
    steps.custom = {
      closure-size = {
        type = "app";
        name = "check-closure-size";
        systems = allowedSystems;
      };
    };
  };
}
