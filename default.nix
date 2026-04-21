# Root composer: same package graph for flake and non-flake consumers.
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
