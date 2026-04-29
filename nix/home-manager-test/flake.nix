{
  description = "Emanote Home Manager module VM test";

  inputs = {
    emanote.url = "github:srid/emanote";
    nixpkgs.follows = "emanote/nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
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
