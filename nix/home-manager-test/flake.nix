{
  description = "Emanote Home Manager module VM test";

  inputs = {
    emanote.url = "path:../..";
    nixpkgs.follows = "emanote/nixpkgs";
    home-manager.follows = "emanote/home-manager";
  };

  outputs = { self, emanote, nixpkgs, home-manager }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      homeManagerModuleTest = import ./default.nix {
        inherit emanote home-manager pkgs;
      };
    in
    {
      checks.${system}.home-manager-module = homeManagerModuleTest;
      packages.${system}.default = homeManagerModuleTest;
    };
}
