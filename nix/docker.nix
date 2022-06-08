# Add a package for docker image on Linux.
{ self, config, lib, flake-parts-lib, ... }:

{
  options = {
    perSystem = flake-parts-lib.mkPerSystemOption
      ({ config, self', inputs', pkgs, system, ... }: {
        packages = pkgs.lib.optionalAttrs (system == "x86_64-linux")
          {
            dockerImage = pkgs.dockerTools.buildImage
              {
                name = "sridca/emanote";
                tag = "latest";
                contents = [
                  self'.packages.default
                  # These are required for the GitLab CI runner
                  pkgs.coreutils
                  pkgs.bash_5
                ];

                config = {
                  WorkingDir = "/data";
                  Volumes = {
                    "/data" = { };
                  };
                  Tmpfs = {
                    "/tmp" = { };
                  };
                  Cmd = [ "${pkgs.lib.getExe self'.packages.default}" ];
                };
              };
          };
      });
  };
}
