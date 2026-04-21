# A flake-parts module for building and running Emanote sites
{ lib, flake-parts-lib, ... }:

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
        options.emanote = mkOption {
          description = "Emanote sites config";
          type = types.submodule {
            options = {
              sites = mkOption {
                description = "Emanote sites";
                type = types.attrsOf (types.submoduleWith {
                  modules = [ ./site ];
                  specialArgs = {
                    inherit pkgs inputs';
                  };
                });
              };
            };
          };
        };
      });
  };
  config = {
    perSystem = { config, self', inputs', pkgs, lib, ... }: {
      packages =
        lib.mapAttrs
          (_: site: site.outputs.package)
          config.emanote.sites;
      apps =
        lib.mapAttrs
          (_: site: site.outputs.app)
          config.emanote.sites;
      checks =
        lib.filterAttrs
          (_: check: check != null)
          (lib.mapAttrs
            (_: site: site.outputs.check)
            config.emanote.sites);
    };
  };
}
