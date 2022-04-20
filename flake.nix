{
  description = "emanote";
  nixConfig = {
    extra-substituters = "https://srid.cachix.org";
    extra-trusted-public-keys = "srid.cachix.org-1:MTQ6ksbfz3LBMmjyPh0PLmos+1x+CdtJxA/J2W+PQxI=";
  };
  inputs = {
    ema.url = "github:srid/ema/multisite";
    nixpkgs.follows = "ema/nixpkgs";
    tailwind-haskell.url = "github:srid/tailwind-haskell/master";
    flake-utils.follows = "ema/flake-utils";
    flake-compat.follows = "ema/flake-compat";

    pathtree.url = "github:srid/pathtree";
    pathtree.flake = false;
    unionmount.url = "github:srid/unionmount/master";
    unionmount.flake = false;
    pandoc-link-context.url = "github:srid/pandoc-link-context/no-dl";
    pandoc-link-context.flake = false;

    # https://github.com/well-typed/ixset-typed/pull/16
    ixset-typed.url = "github:well-typed/ixset-typed";
    ixset-typed.flake = false;

    heist = {
      url = "github:srid/heist/emanote-release--ghc9";
      flake = false;
    };
    lint-utils = {
      type = "git";
      url = "https://gitlab.homotopic.tech/nix/lint-utils.git";
      ref = "overengineered";
      inputs.nixpkgs.follows = "ema/nixpkgs";
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

                path-tree = self.callCabal2nix "path-tree" inputs.pathtree { };
                unionmount = self.callCabal2nix "unionmount" inputs.unionmount { };
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

                    inputs.tailwind-haskell.defaultPackage.${system}
                  ]);
            };
          lintSpec = {
            nixpkgs-fmt = { };
            cabal-fmt = { };
            ormolu = {
              ghcOpts = "-o-XTypeApplications -o-XImportQualifiedPost";
            };
          };
        in
        rec {
          defaultPackage = packages.default;

          # Used by `nix build` & `nix run`
          packages.default = project false;

          # Used by `nix develop`
          devShell = project true;

          # Used by `nix run ...`
          apps = {
            format = inputs.lint-utils.mkApp.${system} lintSpec;
          };

          # Used by `nix flake check`
          checks = {
            format = inputs.lint-utils.mkChecks.${system} lintSpec;
          };
        }) //
    {
      homeManagerModule = import ./home-manager-module.nix;
      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
