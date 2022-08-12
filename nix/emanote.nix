# A flake-parts module for building and running Emanote sites
{ self, config, lib, flake-parts-lib, ... }:

let
  inherit (flake-parts-lib)
    mkSubmoduleOptions
    mkPerSystemOption;
  inherit (lib)
    mkOption
    mkDefault
    types;
  inherit (types)
    functionTo
    raw;
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
                  description = "Emanote package to use";
                  type = types.package;
                };
                sites = mkOption {
                  description = "Emanote sites";
                  type = types.attrsOf (types.submodule {
                    options = {
                      path = mkOption {
                        type = types.path;
                        description = ''Path to the main Emanote layer'';
                      };
                      # HACK: I can't seem to be able to convert `path` to a
                      # relative local path; so this is necessary.
                      #
                      # cf. https://discourse.nixos.org/t/converting-from-types-path-to-types-str/19405?u=srid
                      pathString = mkOption {
                        type = types.str;
                        description = ''Like `path` but local (not in Nix store)'';
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
    perSystem = { config, self', inputs', pkgs, ... }:
      let
        sites =
          lib.mapAttrs
            (name: cfg: {
              app = {
                type = "app";
                # '' is required for escaping ${} in nix
                program = (pkgs.writeShellApplication {
                  name = "emanoteRun.sh";
                  text = ''
                    set -xe
                    cd ${cfg.pathString} 
                    ${config.emanote.package}/bin/emanote run ${if cfg.port == 0 then "" else "--port ${toString cfg.port}"}
                  '';
                }) + /bin/emanoteRun.sh;
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
                in
                pkgs.runCommand "emanote-static-website" { }
                  ''
                    mkdir $out
                    export LANG=C.UTF-8 LC_ALL=C.UTF-8  # https://github.com/EmaApps/emanote/issues/125
                    ${pkgs.lib.getExe config.emanote.package} \
                      --layers "${configDir};${cfg.path}" \
                      ${if cfg.allowBrokenLinks then "--allow-broken-links" else ""} \
                        gen $out
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
