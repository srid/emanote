# A flake-parts module for building and running Emanote sites
{ self, config, lib, flake-parts-lib, ... }:

let
  inherit (flake-parts-lib)
    mkPerSystemOption;
  inherit (lib)
    mkOption
    types;
in
{
  options = {
    perSystem = mkPerSystemOption
      ({ config, self', inputs', pkgs, system, ... }: {
        options.emanote = mkOption
          {
            description = "Emanote sites config";
            type = types.submodule {
              options = {
                package = mkOption {
                  type = types.package;
                  default = inputs'.emanote.packages.default;
                  defaultText = "inputs'.emanote.packages.default";
                  description = ''
                    The emanote package to use. 

                    By default, the 'emanote' flake input will be used.
                  '';
                };
                sites = mkOption {
                  description = "Emanote sites";
                  type = types.attrsOf (types.submodule {
                    options = {
                      layers = mkOption {
                        type = types.listOf types.path;
                        description = ''List of directory paths to run Emanote on'';
                      };
                      # HACK: I can't seem to be able to convert `path` to a
                      # relative local path; so this is necessary.
                      #
                      # cf. https://discourse.nixos.org/t/converting-from-types-path-to-types-str/19405?u=srid
                      layersString = mkOption {
                        type = types.listOf types.str;
                        description = ''Like `layers` but local (not in Nix store)'';
                      };
                      # TODO: Consolidate all these options below with those of home-manager-module.nix
                      port = mkOption {
                        type = types.int;
                        description = ''Port to listen on'';
                        default = 0;
                        defaultText = ''Random port'';
                      };
                      allowBrokenLinks = mkOption {
                        type = types.bool;
                        description = ''Allow broken links in the static site'';
                        default = false;
                      };
                      baseUrl = mkOption {
                        type = types.str;
                        description = ''Base URL for relative links'';
                        default = "/";
                        defaultText = ''Root URL'';
                      };
                      basePath = mkOption {
                        type = types.str;
                        description = ''Top-level directory to copy the static site to'';
                        default = "";
                        defaultText = ''Root path'';
                      };
                      prettyUrls = mkOption {
                        type = types.bool;
                        description = ''Generate links without .html'';
                        default = false;
                      };
                    };
                  });
                };
              };
            };
          };
      });
  };
  config = {
    perSystem = { config, self', inputs', pkgs, lib, ... }:
      let
        sites =
          lib.mapAttrs
            (name: cfg: {
              app = {
                type = "app";

                # '' is required for escaping ${} in nix
                program = pkgs.writeShellApplication {
                  name = "emanote-with-layers-${name}";
                  meta.description = "Live server for Emanote site ${name} (arbitrary arguments accepted)";
                  runtimeInputs = [ config.emanote.package ];
                  text =
                    let
                      layers = lib.concatStringsSep ";" cfg.layersString;
                    in
                    ''
                      set -xe
                      if [ -z "$*" ]; then
                        emanote --layers "${layers}" \
                          run ${if cfg.port == 0 then "" else "--port ${toString cfg.port}"}
                      else
                        emanote --layers "${layers}" \
                          "$@"
                      fi
                    '';
                };
              };
              package =
                let
                  # TODO: Make these options
                  configFile = (pkgs.formats.yaml { }).generate "emanote-index.yaml" {
                    template = {
                      baseUrl = cfg.baseUrl;
                      urlStrategy = if cfg.prettyUrls then "pretty" else "direct";
                    };
                  };
                  configDir = pkgs.runCommand "emanote-deploy-layer" { } ''
                    mkdir -p $out
                    cp ${configFile} $out/index.yaml
                  '';
                  layers = lib.concatStringsSep ";" cfg.layers;
                in
                pkgs.runCommand "emanote-static-website"
                  { meta.description = "Contents of the statically-generated Emanote website for ${name}"; }
                  ''
                    mkdir $out
                    export LANG=C.UTF-8 LC_ALL=C.UTF-8  # https://github.com/srid/emanote/issues/125
                    ${pkgs.lib.getExe config.emanote.package} \
                      --layers "${configDir};${layers}" \
                      ${if cfg.allowBrokenLinks then "--allow-broken-links" else ""} \
                        gen $out/${cfg.basePath}
                  '';
            })
            config.emanote.sites;
      in
      {
        packages =
          lib.mapAttrs
            (_: site: site.package)
            sites;
        apps =
          lib.mapAttrs
            (_: site: site.app)
            sites;
      };
  };
}
