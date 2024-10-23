{ inputs, ... }:
{
  imports = [
    inputs.flake-root.flakeModule
    inputs.treefmt-nix.flakeModule
  ];

  perSystem = { pkgs, lib, config, system, ... }: {
    # Autoformatter configuration: https://nixos.asia/en/treefmt
    treefmt.config = {
      inherit (config.flake-root) projectRootFile;
      package = pkgs.treefmt;

      programs.ormolu.enable = true;
      programs.nixpkgs-fmt.enable = true;
      programs.cabal-fmt.enable = true;
      programs.hlint.enable = true;

      # We use fourmolu
      programs.ormolu.package = pkgs.haskellPackages.fourmolu;
      settings.formatter.ormolu = {
        options = [
          "--ghc-opt"
          "-XImportQualifiedPost"
          "--ghc-opt"
          "-XTypeApplications"
        ];
      };
    };
 };
}
