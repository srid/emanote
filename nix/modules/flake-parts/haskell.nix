{ root, inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
  ];

  perSystem = { pkgs, lib, config, system, ... }: {
    # haskell-flake configuration
    haskellProjects.default = {
      projectFlakeName = "emanote";
      projectRoot = root;
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
        pandoc-link-context.broken = false;
        pandoc-link-context.jailbreak = true;
        tagtree.broken = false;
        tagtree.jailbreak = true;
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

    packages.default = config.packages.emanote;
    apps.default = config.apps.emanote;
    apps.check-closure-size = rec {
      inherit (program) meta;
      program = pkgs.writeShellApplication {
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
    };
  };
}
