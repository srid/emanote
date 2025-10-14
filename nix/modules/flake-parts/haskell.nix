{ root, inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
  ];

  perSystem = { pkgs, lib, config, system, ... }: {
    # haskell-flake configuration
    haskellProjects.default = {
      projectFlakeName = "emanote";

      # Whitelist to avoid unnecessary rebuilds of Nix package.
      projectRoot = builtins.toString (lib.fileset.toSource {
        inherit root;
        fileset = lib.fileset.unions [
          (root + /emanote)
          (root + /cabal.project)
        ];
      });

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

        ema.source = inputs.ema + /ema;
        ema-generics.source = inputs.ema + /ema-generics;
        ema-extra.source = inputs.ema + /ema-extra;
        lvar.source = inputs.lvar;
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

    # Static binary built with musl (Linux only)
    packages.emanote-static =
      if pkgs.stdenv.isLinux then
        let
          muslPkgs = pkgs.pkgsMusl;
          muslHaskellPackages = muslPkgs.haskell.packages.ghc98;
        in
        muslHaskellPackages.callCabal2nix "emanote"
          (root + /emanote)
          {
            # stork = muslPkgs.stork;
          }
      else
        config.packages.emanote;

    apps.default = config.apps.emanote;
  };
}
