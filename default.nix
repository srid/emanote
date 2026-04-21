# Root composer for Emanote Nix outputs.
#
# Used by flake.nix (thin wrapper), by `nix-build` directly, and by shell.nix.
# Emits the same package graph whether the caller uses flakes or traditional
# Nix, so there is exactly one build definition.
{ pkgs ? import ./nix/nixpkgs.nix { }
, project ? import ./nix/haskell-project.nix { inherit pkgs; }
}:
let
  emanote = project.packages.emanote.package;

  mkSite = import ./nix/emanote-site.nix { inherit pkgs; };

  docs = mkSite {
    name = "docs";
    package = emanote;
    layers = [{ path = ./docs; pathString = "./docs"; }];
    allowBrokenInternalLinks = true; # A couple, by design, in markdown.md
    extraConfig.template.urlStrategy = "pretty";
  };

  closureSizeCheck = import ./nix/check-closure-size.nix { inherit pkgs emanote; };
in
{
  inherit emanote project;
  default = emanote;
  docs = docs.package;
  docs-app = docs.app;
  docs-linkCheck = docs.check;
  closure-size = closureSizeCheck;
}
