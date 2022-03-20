{
  description = "emanote";
  inputs = {
    ema.url = "github:srid/ema/multisite";
    # Use the nixpkgs used by the pinned ema.
    tailwind-haskell.url = "github:srid/tailwind-haskell/master";
    nixpkgs.follows = "ema/nixpkgs";
    tailwind-haskell.inputs.nixpkgs.follows = "ema/nixpkgs";
    tailwind-haskell.inputs.flake-utils.follows = "ema/flake-utils";
    tailwind-haskell.inputs.flake-compat.follows = "ema/flake-compat";
    flake-utils.follows = "/ema/flake-utils";
    flake-compat.follows = "/ema/flake-compat";

    pathtree.url = "github:srid/pathtree";
    pathtree.inputs.nixpkgs.follows = "ema/nixpkgs";
    unionmount.url = "github:srid/unionmount/master";
    unionmount.inputs.nixpkgs.follows = "ema/nixpkgs";

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
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
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
          project = returnShellEnv:
            pkgs.haskellPackages.developPackage {
              inherit returnShellEnv;
              name = "emanote";
              root = pkgs.lib.cleanSourceWith { inherit filter; src = ./.; name = "emanote"; };
              withHoogle = true;
              overrides = self: super: with pkgs.haskell.lib; {
                ema = inputs.ema.defaultPackage.${system};
                tailwind = inputs.tailwind-haskell.defaultPackage.${system};

                pathtree = inputs.pathtree.defaultPackage.${system};
                unionmount = inputs.unionmount.defaultPackage.${system};
                relude = self.callHackage "relude" "1.0.0.1" { }; # Not on nixpkgs, for some reason.
                # commonmark-simple = inputs.commonmark-simple.defaultPackage.${system};
                # url-slug = inputs.url-slug.defaultPackage.${system};
                # tagtree = self.callCabal2nix "tagtree" inputs.tagtree { };
                # lvar = self.callCabal2nix "lvar" inputs.ema.inputs.lvar { }; # Until lvar gets into nixpkgs

                # Jailbreak heist to allow newer dlist
                heist = doJailbreak (dontCheck (self.callCabal2nix "heist" inputs.heist { }));
              };
              modifier = drv:
                pkgs.haskell.lib.addBuildTools drv
                  (with pkgs.haskellPackages; pkgs.lib.lists.optionals returnShellEnv [
                    # Specify your build/dev dependencies here. 
                    cabal-fmt
                    cabal-install
                    ghcid
                    haskell-language-server
                    ormolu
                    pkgs.nixpkgs-fmt

                    inputs.tailwind-haskell.defaultPackage.${system}
                  ]);
            };
        in
        {
          # Used by `nix build` & `nix run`
          defaultPackage = project false;

          # Used by `nix develop`
          devShell = project true;
        }) //
    {
      homeManagerModule = import ./home-manager-module.nix;
    };
}
