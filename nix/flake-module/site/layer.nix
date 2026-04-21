{ config, lib, ... }:

let
  inherit (lib)
    mkOption
    types;
in
{
  options = {
    path = mkOption {
      type = types.path;
      description = ''Directory path to notes'';
    };
    # HACK: I can't seem to be able to convert `path` to a
    # relative local path; so this is necessary.
    #
    # cf. https://discourse.nixos.org/t/converting-from-types-path-to-types-str/19405?u=srid
    pathString = mkOption {
      type = types.str;
      description = ''Like `path` but local (not in Nix store)'';
      default = builtins.toString config.path;
      defaultText = lib.literalMD "toString path";
    };
    mountPoint = mkOption {
      type = types.nullOr types.str;
      description = ''Mount point for the layer'';
      default = null;
    };
    outputs.layer = mkOption {
      type = types.str;
      description = ''Layer spec'';
      readOnly = true;
    };
    outputs.layerString = mkOption {
      type = types.str;
      description = ''Layer spec'';
      readOnly = true;
    };
  };
  config = {
    outputs.layer =
      if config.mountPoint == null then "${config.path}" else "${config.path}@${config.mountPoint}";
    outputs.layerString =
      if config.mountPoint == null then config.pathString else "${config.pathString}@${config.mountPoint}";
  };
}
