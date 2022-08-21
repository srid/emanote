{ ... }:

{
  perSystem = { system, pkgs, ... }: {
    packages.stork =
      # Stork is marked as broken on intel mac, but it does work.
      # Unfortunately we cannot test this code PATH due to lack of CI for intel mac (#335).
      if system == "x86_64-darwin" then pkgs.stork.overrideAttrs (_oa: { meta.broken = false; }) else pkgs.stork;
  };
}
