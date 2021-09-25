{
  description = "emanote";
  inputs = {
    # Pick the succeeding rev from https://github.com/input-output-hk/haskell.nix/commits/master
    haskellNix.url = "github:input-output-hk/haskell.nix/f5ec5311fa805b46f5276e2b4f574c8e7544a30b";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    ema.url = "github:srid/ema/master";
    windicss = {
      url = "github:srid/windicss-nix";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, haskellNix, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            emanote =
              final.haskell-nix.cabalProject'
                {
                  src = final.haskell-nix.haskellLib.cleanSourceWith { inherit filter; src = ./.; name = "emanote"; };
                  compiler-nix-name = "ghc8107";
                  shell.tools = {
                    cabal = { };
                    hlint = { };
                    ghcid = { };
                    # ormolu = { }; -- this compiles ghc-lib-parser!
                    haskell-language-server = { };
                  };
                  shell.buildInputs = [
                    pkgs.nixpkgs-fmt
                    pkgs.haskellPackages.cabal-fmt
                    pkgs.ormolu
                  ];
                };
          })
        ];
        pkgs =
          import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        windicss =
          (import inputs.windicss { inherit pkgs; }).shell.nodeDependencies;
        # Based on https://github.com/input-output-hk/daedalus/blob/develop/yarn2nix.nix#L58-L71
        filter = name: type:
          let
            baseName = baseNameOf (toString name);
            sansPrefix = pkgs.lib.removePrefix (toString ./.) name;
          in
          # Ignore these files when building emanote source package
            !(
              baseName == "README.md" ||
              sansPrefix == "/bin" ||
              sansPrefix == "/docs" ||
              sansPrefix == "/.github" ||
              sansPrefix == "/.vscode"
            );
        flake = pkgs.emanote.flake { };
      in
      flake // {
        # Used by `nix build` & `nix run`
        defaultPackage = flake.packages."emanote:exe:emanote";

        # Repl: nix run .#repl
        apps = {
          repl = flake-utils.lib.mkApp {
            drv = pkgs.writeShellScriptBin "repl" ''
              confnix=$(mktemp)
              echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
              trap "rm $confnix" EXIT
              nix repl $confnix
            '';
          };
        };

        inherit windicss;
      });
}
