{
  description = "emanote: Emanate a structured view of your plain-text notes";
  nixConfig = {
    extra-substituters = "https://cache.nixos.asia/oss";
    extra-trusted-public-keys = "oss:KO872wNJkCDgmGN3xy9dT89WAhvv13EiKncTtHDItVU=";
  };
  inputs = { };
  outputs = _:
    let
      sources = import ./npins;
      lib = import (sources.nixpkgs + /lib);
      systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      forAllSystems = lib.genAttrs systems;

      pkgsFor = system: import sources.nixpkgs {
        inherit system;
        overlays = [
          (_: super: {
            stork =
              if system == "x86_64-darwin"
              then super.stork.overrideAttrs (_: { meta.broken = false; })
              else super.stork;
          })
        ];
      };

      haskellProjectFor = pkgs:
        (import (sources.haskell-flake + /nix/lib.nix) { inherit pkgs; }).evalHaskellProject {
          projectRoot = builtins.toString (lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              ./emanote
              ./cabal.project
            ];
          });
          modules = [{
            packages = {
              unionmount.source = sources.unionmount;
              commonmark-simple.source = sources.commonmark-simple;
              commonmark-wikilink.source = sources.commonmark-wikilink;
              fsnotify.source = "0.4.1.0";
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
              tailwind.broken = false;
              tailwind.jailbreak = true;
              unionmount = { pkgs, ... }: { check = !pkgs.stdenv.isDarwin; };
              emanote = { pkgs, ... }: {
                check = false;
                extraBuildDepends = [ pkgs.stork ];
                custom = pkg: pkg.overrideAttrs (lib.addMetaAttrs {
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
          }];
        };

      preCommitFor = pkgs:
        let
          gitHooks = import (sources.git-hooks + /nix) {
            nixpkgs = sources.nixpkgs;
            gitignore-nix-src = sources.gitignore-nix;
            isFlakes = true;
            inherit (pkgs) system;
          };
        in
        gitHooks.run {
          src = ./.;
          hooks = {
            nixpkgs-fmt.enable = true;
            fourmolu = {
              enable = true;
              excludes = [ "vira\\.hs" ];
            };
            hlint = {
              enable = true;
              excludes = [ "vira\\.h" ];
            };
            cabal-fmt.enable = true;
          };
        };

      buildDocsSite = pkgs: emanotePackage:
        let
          configFile = (pkgs.formats.yaml { }).generate "emanote-index.yaml" {
            template.urlStrategy = "pretty";
          };
          configDir = pkgs.runCommand "emanote-deploy-layer" { } ''
            mkdir -p $out
            cp ${configFile} $out/index.yaml
          '';
        in
        pkgs.runCommand "emanote-static-website-docs"
          { meta.description = "Contents of the statically-generated Emanote website for docs"; }
          ''
            mkdir -p $out
            export LANG=C.UTF-8 LC_ALL=C.UTF-8
            ${lib.getExe emanotePackage} \
              --layers "${configDir};${./docs}" \
              --allow-broken-internal-links \
              gen $out
          '';

      docsApp = pkgs: emanotePackage: {
        type = "app";
        program = lib.getExe (pkgs.writeShellApplication {
          name = "emanote-docs";
          meta.description = "Live server for Emanote site docs";
          runtimeInputs = [ emanotePackage ];
          text = ''
            set -xe
            if [ -z "$*" ]; then
              emanote --layers "${./docs}" run
            else
              emanote --layers "${./docs}" "$@"
            fi
          '';
        });
      };

    in
    {
      packages = forAllSystems (system:
        let
          pkgs = pkgsFor system;
          project = haskellProjectFor pkgs;
        in
        {
          emanote = project.packages.emanote.package;
          default = project.packages.emanote.package;
          docs = buildDocsSite pkgs project.packages.emanote.package;
        });

      apps = forAllSystems (system:
        let
          pkgs = pkgsFor system;
          project = haskellProjectFor pkgs;
        in
        {
          emanote = { type = "app"; program = lib.getExe project.packages.emanote.package; };
          default = { type = "app"; program = lib.getExe project.packages.emanote.package; };
          docs = docsApp pkgs project.packages.emanote.package;
        });

      checks = forAllSystems (system:
        let
          pkgs = pkgsFor system;
          preCommit = preCommitFor pkgs;
          project = haskellProjectFor pkgs;
          docsSite = buildDocsSite pkgs project.packages.emanote.package;
        in
        {
          pre-commit = preCommit;
          docs-linkCheck = pkgs.runCommand "emanote-docs-linkCheck"
            {
              buildInputs = [ pkgs.html-proofer ];
              meta.description = "Check links in the statically-generated Emanote website for docs";
            } ''
            export LANG=C.UTF-8 LC_ALL=C.UTF-8
            export RUBYOPT="-E utf-8"
            htmlproofer --disable-external ${docsSite}
            touch $out
          '';
        });

      devShells = forAllSystems (system:
        let
          pkgs = pkgsFor system;
          project = haskellProjectFor pkgs;
          preCommit = preCommitFor pkgs;
        in
        {
          default = lib.addMetaAttrs
            { description = "Emanote development environment"; }
            (pkgs.mkShell {
              name = "emanote-dev";
              inputsFrom = [ project.devShell ];
              shellHook = preCommit.shellHook;
              packages = preCommit.enabledPackages ++ [ pkgs.just pkgs.stork ];
            });
        });

      # Exported for downstream consumers (these are still flake-parts modules)
      flakeModule = ./nix/modules/flake-parts/flake-module;
      homeManagerModule = import ./nix/modules/home/emanote.nix;
      templates.default = {
        description = "A simple flake.nix template for emanote notebooks";
        path = builtins.path {
          path = sources.emanote-template;
          filter = path: _: baseNameOf path == "flake.nix";
        };
      };
    };
}
