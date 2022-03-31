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
    tailwind-haskell.inputs.nixpkgs.follows = "ema/nixpkgs";
    tailwind-haskell.inputs.flake-utils.follows = "ema/flake-utils";
    tailwind-haskell.inputs.flake-compat.follows = "ema/flake-compat";
    flake-utils.follows = "ema/flake-utils";
    flake-compat.follows = "ema/flake-compat";

    pathtree.url = "github:srid/pathtree";
    pathtree.inputs.nixpkgs.follows = "ema/nixpkgs";
    unionmount.url = "github:srid/unionmount/master";
    unionmount.inputs.nixpkgs.follows = "ema/nixpkgs";

    heist = {
      url = "github:srid/heist/emanote";
      flake = false;
    };
    lint-utils = {
      type = "git";
      url = "https://gitlab.homotopic.tech/nix/lint-utils.git";
      ref = "parameterized";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          # Based on https://github.com/input-output-hk/daedalus/blob/develop/yarn2nix.nix#L58-L71
          fourmoluOpts = "-o-XTypeApplications -o-XImportQualifiedPost";
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
                relude = self.relude_1_0_0_1;
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
          # Checks the shell script using ShellCheck
          checkedShellScript = name: text:
            (pkgs.writeShellApplication {
              inherit name text;
            }) + "/bin/${name}";

          # Concat a list of Flake apps to produce a new app that runs all of them
          # in sequence.
          concatApps = apps:
            {
              type = "app";
              program = checkedShellScript "concatApps"
                (pkgs.lib.strings.concatMapStringsSep
                  "\n"
                  (app: app.program)
                  apps);
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
            format = concatApps [
              (inputs.lint-utils.apps.${system}.fourmolu fourmoluOpts)
              inputs.lint-utils.apps.${system}.cabal-fmt
              inputs.lint-utils.apps.${system}.nixpkgs-fmt
            ];
          };

          # Used by `nix flake check` (but see next attribute)
          checks = {
            format-haskell = inputs.lint-utils.linters.${system}.fourmolu ./. fourmoluOpts;
            format-cabal = inputs.lint-utils.linters.${system}.cabal-fmt ./.;
            format-nix = inputs.lint-utils.linters.${system}.nixpkgs-fmt ./.;
          };
        }) //
    {
      homeManagerModule = import ./home-manager-module.nix;
      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
