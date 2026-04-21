# Dev shell — shared by `nix develop` (via flake.nix) and `nix-shell`.
{ pkgs ? import ./nix/nixpkgs.nix { } }:
let
  project = import ./nix/haskell-project.nix { inherit pkgs; };
in
pkgs.mkShell {
  name = "emanote-dev";
  inputsFrom = [ project.devShell ];
  packages = with pkgs; [
    just
    nixpkgs-fmt
  ];
}
