{ inputs, self, ... }:
{
  perSystem = { lib, pkgs, ... }: lib.mkIf pkgs.stdenv.isLinux {
    checks.home-manager-module = import ../../home-manager-test {
      emanote = self;
      home-manager = inputs.home-manager;
      inherit pkgs;
    };
  };
}
