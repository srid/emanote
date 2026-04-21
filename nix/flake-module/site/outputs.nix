{ config, lib, name, pkgs, ... }:

let
  mkSite = import ../../emanote-site.nix { inherit pkgs; };
  site = mkSite {
    inherit name;
    inherit (config) package layers basePath allowBrokenInternalLinks extraConfig check port;
  };
in
{
  options.outputs = {
    package = lib.mkOption {
      type = lib.types.package;
      description = "The static website package";
      readOnly = true;
    };
    app = lib.mkOption {
      type = lib.types.attrs;
      description = "The live server app";
      readOnly = true;
    };
    check = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      description = "Link check for the static website";
      readOnly = true;
    };
  };
  config.outputs = {
    inherit (site) package app check;
  };
}
