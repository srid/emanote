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
                      layers = lib.mkOption {
                        description = "List of layers to use for the site";
                        type = types.listOf (types.submodule ({ config, ... }: {
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
                            };
                          };
                        }));
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
                      layers = lib.concatStringsSep ";" (builtins.map (x: x.pathString) cfg.layers);
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
                  layers = lib.concatStringsSep ";" (builtins.map (x: x.path) cfg.layers);
                in
                pkgs.runCommand "emanote-static-website-${name}"
                  { meta.description = "Contents of the statically-generated Emanote website for ${name}"; }
                  ''
                    OUTPATH=$out/${cfg.basePath}
                    mkdir -p $OUTPATH
                    export LANG=C.UTF-8 LC_ALL=C.UTF-8  # https://github.com/srid/emanote/issues/125
                    ${pkgs.lib.getExe config.emanote.package} \
                      --layers "${configDir};${layers}" \
                      ${if cfg.allowBrokenLinks then "--allow-broken-links" else ""} \
                        gen $OUTPATH
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
