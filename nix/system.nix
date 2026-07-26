{ root, sources, system }:
let
  pkgs = import sources.nixpkgs {
    inherit system;
    overlays = [
      (_final: previous: {
        stork = if system == "x86_64-darwin" then previous.stork.overrideAttrs (_: { meta.broken = false; }) else previous.stork;
      })
    ];
  };
  inherit (pkgs) lib;
  diagrams = import ./diagrams.nix { inherit pkgs; };
  runtimeProject = import ./project.nix {
    inherit diagrams pkgs root sources;
    withDevShell = false;
  };
  devProject = import ./project.nix { inherit diagrams pkgs root sources; };
  preCommit = import ./pre-commit.nix { inherit pkgs root sources; };
  emanote = runtimeProject.packages.emanote.package;
  docs = (lib.evalModules {
    modules = [
      ./modules/flake-parts/flake-module/site
      {
        package = emanote;
        layers = [{ path = root + /docs; pathString = "./docs"; }];
        allowBrokenInternalLinks = true;
        allowBrokenLuaFilters = true;
        extraConfig.template.urlStrategy = "pretty";
      }
    ];
    specialArgs = {
      inherit pkgs;
      name = "docs";
      inputs' = { };
    };
  }).config.outputs;
in
{
  packages = {
    default = emanote;
    inherit emanote;
    docs = docs.package;
  };
  apps = {
    default = runtimeProject.apps.emanote;
    emanote = runtimeProject.apps.emanote;
    docs = docs.app // { program = lib.getExe docs.app.program; };
  };
  devShells = {
    default = lib.addMetaAttrs { description = "Emanote development environment"; }
      (pkgs.mkShell {
        name = "emanote-dev";
        inputsFrom = [ devProject.devShell diagrams.devShell ];
        packages = [ pkgs.just ];
      });
    diagrams = diagrams.devShell;
    fmt = lib.addMetaAttrs { description = "Emanote formatting environment"; }
      (pkgs.mkShell {
        name = "emanote-fmt";
        inputsFrom = [ preCommit.devShell ];
      });
  };
  checks = {
    diagrams-typst-offline = diagrams.check;
    docs = docs.check;
    pre-commit = preCommit.check;
  };
}
