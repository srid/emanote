{
  description = "emanote";
  inputs = {
    ema.url = "github:srid/ema/master";
    # Use the nixpkgs used by the pinned ema.
    nixpkgs.follows = "ema/nixpkgs";
    tailwind = {
      url = "github:srid/tailwind-nix";
      flake = false;
    };
    pandoc-link-context = {
      url = "github:srid/pandoc-link-context";
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
        tailwindNix =
          (import inputs.tailwind { inherit pkgs; }).shell.nodeDependencies;
        project = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            inherit returnShellEnv;
            name = "emanote";
            root = ./.;
            withHoogle = false;
            overrides = self: super: with pkgs.haskell.lib; {
              ema = disableCabalFlag inputs.ema.defaultPackage.${system} "with-examples";
              pandoc-link-context = self.callCabal2nix "pandoc-link-context" inputs.pandoc-link-context { };
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

                tailwindNix
              ]);
          };
      in
      {
        # Used by `nix build` & `nix run`
        defaultPackage = project false;

        # Used by `nix develop`
        devShell = project true;
      });
}
