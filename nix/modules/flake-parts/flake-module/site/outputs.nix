{ config, lib, name, pkgs, ... }:

let
  inherit (lib)
    mkOption
    types;
in
{
  options = {
    outputs.package = mkOption {
      type = types.package;
      description = ''The static website package'';
      readOnly = true;
    };
    outputs.app = mkOption {
      type = types.attrs;
      description = ''The live server app'';
      readOnly = true;
    };
    outputs.check = mkOption {
      type = types.nullOr types.package;
      description = ''Link check for the static website'';
      readOnly = true;
    };
  };
  config = {
    outputs.app = rec {
      type = "app";

      inherit (program) meta;

      # '' is required for escaping ${} in nix
      program = pkgs.writeShellApplication {
        name = "emanote-with-layers-${name}";
        meta.description = "Live server for Emanote site ${name}";
        runtimeInputs = [ config.package ];
        text =
          let
            layers = lib.concatStringsSep ";" (builtins.map (x: x.outputs.layerString) config.layers);
          in
          ''
            set -xe
            if [ -z "$*" ]; then
              emanote --layers "${layers}" \
                run ${if config.port == 0 then "" else "--port ${toString config.port}"}
            else
              emanote --layers "${layers}" \
                "$@"
            fi
          '';
      };
    };
    outputs.package =
      let
        configFile = (pkgs.formats.yaml { }).generate "emanote-index.yaml" config.extraConfig;
        configDir = pkgs.runCommand "emanote-deploy-layer" { } ''
          mkdir -p $out
          cp ${configFile} $out/index.yaml
        '';
        layers = lib.concatStringsSep ";" (builtins.map (x: x.outputs.layer) config.layers);
      in
      pkgs.runCommand "emanote-static-website-${name}"
        { meta.description = "Contents of the statically-generated Emanote website for ${name}"; }
        ''
          OUTPATH=$out/${config.basePath}
          mkdir -p $OUTPATH
          export LANG=C.UTF-8 LC_ALL=C.UTF-8  # https://github.com/srid/emanote/issues/125
          ${pkgs.lib.getExe config.package} \
            --layers "${configDir};${layers}" \
            ${if config.allowBrokenInternalLinks then "--allow-broken-internal-links" else ""} \
              gen $OUTPATH
        '';
    outputs.check =
      if config.check then
        pkgs.runCommand "emanote-${name}-linkCheck"
          {
            buildInputs = [ pkgs.html-proofer ];
            meta.description = "Check links in the statically-generated Emanote website for ${name}";
          } ''
          # Ensure that the htmlproofer is using the correct locale and encoding
          export LANG=C.UTF-8 LC_ALL=C.UTF-8
          export RUBYOPT="-E utf-8"
          # Run htmlproofer
          htmlproofer --disable-external ${config.outputs.package}
          touch $out
        ''
      else
        null;
  };
}
