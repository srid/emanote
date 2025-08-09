{ config, lib, name, pkgs, inputs', ... }:

let
  inherit (lib)
    mkOption
    types;
in
{
  imports = [
    ./outputs.nix
  ];

  options = {
    package = mkOption {
      type = types.package;
      default = inputs'.emanote.packages.default;
      defaultText = "inputs'.emanote.packages.default";
      description = ''The emanote package to use'';
    };
    layers = lib.mkOption {
      description = "List of layers to use for the site";
      type = types.listOf (types.submodule ./layer.nix);
    };
    # TODO: Consolidate all these options below with those of home-manager-module.nix
    port = mkOption {
      type = types.int;
      description = ''Port to listen on'';
      default = 0;
      defaultText = ''Random port'';
    };
    allowBrokenInternalLinks = mkOption {
      type = types.bool;
      description = ''Allow broken internal links in the static site'';
      default = false;
    };
    basePath = mkOption {
      type = types.str;
      description = ''Top-level directory to copy the static site to'';
      default = "";
      defaultText = ''Root path'';
    };

    extraConfig = mkOption {
      type = types.attrs;
      description = ''Extra configuration to be merged into index.yaml'';
      default = { };
    };

    check = mkOption {
      type = types.bool;
      description = ''Whether to enable flake checks (htmlproofer) for this site'';
      default = true;
    };
  };
}
