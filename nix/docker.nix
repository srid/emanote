# Add a package for docker image on Linux.
{ ... }:

{
  config = {
    perSystem = { config, self', inputs', pkgs, system, ... }: {
      packages = pkgs.lib.optionalAttrs (system == "x86_64-linux")
        {
          dockerImage = pkgs.dockerTools.buildImage
            {
              name = "sridca/emanote";
              tag = "latest";
              copyToRoot = pkgs.buildEnv {
                name = "image-root";
                paths = [
                  self'.packages.default
                  # These are required for the GitLab CI runner
                  pkgs.coreutils
                  pkgs.bash_5
                ];
                pathsToLink = [ "/bin" ];
              };
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
    };
  };
}
