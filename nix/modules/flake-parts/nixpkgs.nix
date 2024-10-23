{ inputs, ... }:
{
  perSystem = { pkgs, lib, config, system, ... }: {
    _module.args = import inputs.nixpkgs {
      inherit system;
      overlays = [
        (self: super: {
          # Stork is marked as broken on intel mac, but it does work.
          # Unfortunately we cannot test this code PATH due to lack of CI for intel mac (#335).
          stork = if system == "x86_64-darwin" then super.stork.overrideAttrs (_oa: { meta.broken = false; }) else super.stork;
        })
      ];
    };
  };
}
