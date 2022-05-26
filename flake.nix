{
  description = "emanote";
  nixConfig = {
    extra-substituters = "https://cache.garnix.io";
    extra-trusted-public-keys = "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g=";
  };
  inputs = {
    ema.url = "github:srid/ema/multisite";
    nixpkgs.follows = "ema/nixpkgs";
    tailwind-haskell.url = "github:srid/tailwind-haskell/master";
    tailwind-haskell.inputs.ema.follows = "ema";
    flake-utils.follows = "ema/flake-utils";
    flake-compat.follows = "ema/flake-compat";

    pandoc-link-context.url = "github:srid/pandoc-link-context/master";
    pandoc-link-context.flake = false;

    # 0.5.1.0 is broken on nixpkgs
    ixset-typed.url = "github:well-typed/ixset-typed";
    ixset-typed.flake = false;

    heist = {
      url = "github:srid/heist/emanote";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          project = returnShellEnv:
            pkgs.haskellPackages.developPackage {
              inherit returnShellEnv;
              name = "emanote";
              root = ./.;
              withHoogle = true;
              overrides = self: super: with pkgs.haskell.lib; {
                ema = inputs.ema.defaultPackage.${system};
                tailwind = inputs.tailwind-haskell.defaultPackage.${system};

                pandoc-link-context = self.callCabal2nix "pandoc-link-context" inputs.pandoc-link-context { };

                # Jailbreak heist to allow newer dlist
                heist-emanote = doJailbreak (dontCheck (self.callCabal2nix "heist-emanote" inputs.heist { }));
                ixset-typed = doJailbreak (dontCheck (self.callCabal2nix "ixset-typed" inputs.ixset-typed { }));
              };
              modifier = drv:
                pkgs.haskell.lib.addBuildTools drv
                  (with pkgs.haskellPackages; pkgs.lib.lists.optionals returnShellEnv [
                    # Specify your build/dev dependencies here. 
                    cabal-install
                    ghcid
                    haskell-language-server

                    # Auto-formatters (used by editors, in nix-shell)
                    cabal-fmt
                    ormolu
                    pkgs.nixpkgs-fmt
                    pkgs.treefmt

                    inputs.tailwind-haskell.defaultPackage.${system}
                  ]);
            };
        in
        rec {
          defaultPackage = packages.default;

          # Used by `nix build` & `nix run`
          packages.default = project false;

          # Used by `nix develop`
          devShell = project true;
        }) //
    {
      homeManagerModule = import ./home-manager-module.nix;
      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
