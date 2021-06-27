{
  description = "emanote";
  inputs = {
    ema.url = "github:srid/ema/non-pretty-url";
    # Use the nixpkgs used by the pinned ema.
    nixpkgs.follows = "ema/nixpkgs";
    windicss = {
      url = "github:srid/windicss-nix";
      flake = false;
    };
    pandoc-link-context = {
      url = "github:srid/pandoc-link-context";
      flake = false;
    };
    tagtree = {
      url = "github:srid/tagtree";
      flake = false;
    };
    heist = {
      url = "github:srid/heist/emanote";
      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlays = [ ];
        pkgs =
          import nixpkgs { inherit system overlays; config.allowBroken = true; };
        windicss =
          (import inputs.windicss { inherit pkgs; }).shell.nodeDependencies;
        project = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            inherit returnShellEnv;
            name = "emanote";
            root = ./.;
            withHoogle = true;
            overrides = self: super: with pkgs.haskell.lib; {
              ema = disableCabalFlag inputs.ema.defaultPackage.${system} "with-examples";
              pandoc-link-context = self.callCabal2nix "pandoc-link-context" inputs.pandoc-link-context { };
              tagtree = self.callCabal2nix "tagtree" inputs.tagtree { };
              heist = dontCheck (self.callCabal2nix "heist" inputs.heist { });
              # lvar = self.callCabal2nix "lvar" inputs.ema.inputs.lvar { }; # Until lvar gets into nixpkgs
            };
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
              [
                cabal-fmt
                cabal-install
                ghcid
                haskell-language-server
                ormolu
                pkgs.nixpkgs-fmt

                windicss
              ]);
          };
      in
      {
        # Used by `nix build` & `nix run`
        defaultPackage = project false;

        inherit windicss;

        # Used by `nix develop`
        devShell = project true;
      });
}
