# haskell-flake project definition, consumed in standalone mode by
# default.nix and shell.nix via `haskell-flake.lib.evalHaskellProject`.
{ pkgs }:
let
  sources = import ../npins;

  projectRoot = pkgs.lib.fileset.toSource {
    root = ../.;
    fileset = pkgs.lib.fileset.unions [
      ../emanote
      ../cabal.project
    ];
  };

  haskellFlakeLib = import (sources.haskell-flake + /nix/lib.nix) { inherit pkgs; };
in
haskellFlakeLib.evalHaskellProject {
  name = "emanote";
  inherit projectRoot;
  modules = [
    ({ lib, ... }: {
      projectFlakeName = "emanote";

      devShell.tools = hp: {
        inherit (pkgs) stork tailwindcss_4;
        inherit (hp) cabal-fmt;
        # hlint is useful during development; fourmolu is kept out since we
        # no longer enforce formatting.
      };

      packages = {
        unionmount.source = sources.unionmount;
        commonmark-simple.source = sources.commonmark-simple;
        commonmark-wikilink.source = sources.commonmark-wikilink;
        fsnotify.source = "0.4.1.0"; # Not in nixpkgs, yet.
        ghcid.source = "0.8.8";
        heist-extra.source = sources.heist-extra;

        ema.source = sources.ema + /ema;
        ema-generics.source = sources.ema + /ema-generics;
        ema-extra.source = sources.ema + /ema-extra;
        lvar.source = sources.lvar;
      };

      settings = {
        fsnotify.check = false;
        heist.broken = false;
        ixset-typed.broken = false;
        ixset-typed.jailbreak = true;
        pandoc-link-context.broken = false;
        pandoc-link-context.jailbreak = true;
        tagtree.broken = false;
        tagtree.jailbreak = true;
        unionmount.check = !pkgs.stdenv.isDarwin; # garnix: Slow M1 builder
        emanote = { ... }: {
          check = false;
          extraBuildDepends = [ pkgs.stork pkgs.tailwindcss_4 ];
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
    })
  ];
}
