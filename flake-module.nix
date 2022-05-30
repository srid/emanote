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
                        default = "${self}";
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
                    # Use `emanote run --port=8081` if you want to run Emanote at
                    # a particular port.
                    cd ${cfg.path} && ${config.emanote.package}/bin/emanote
                  '';
                }) + /bin/emanoteRun.sh;
              };
              package =
                let
                  # TODO: Make these options
                  configFile = (pkgs.formats.yaml { }).generate "emanote-index.yaml" {
                    template = {
                      baseUrl = "/";
                      urlStrategy = "direct";
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
                    ${config.emanote.package}/bin/emanote \
                    --layers "${configDir};${cfg.path}" \
                      gen $out
                  '';
            })
            config.emanote.sites;
        # Inject a 'default' attr if the attrset is a singleton set.
        withDefault = attrs:
          let xs = lib.attrValues attrs; in
          if builtins.length xs == 1
          then attrs // { default = mkDefault (builtins.head xs); }
          else attrs;
      in
      {
        packages =
          withDefault (lib.mapAttrs
            (_: site: site.package)
            sites);
        apps =
          withDefault (lib.mapAttrs
            (_: site: site.app)
            sites);
      };
  };
}
