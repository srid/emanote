# TODO: Make a flake-parts module out of this, for publish container images built in Nix to ghcr.io
{ root, ... }: {
  perSystem = { pkgs, config, lib, ... }:
    let
      emanote = config.packages.emanote;
      container-name = "ghcr.io/srid/emanote";
      container = pkgs.dockerTools.buildLayeredImage {
        name = container-name;
        tag = "latest";
        created = "now";
        config.Entrypoint = [ "${emanote}/bin/emanote" ];
        config.WorkingDir = "/notebook";
        config.Env = [ "LANG=C.UTF-8" ];
        config.Labels = {
          "org.opencontainers.image.source" = "https://github.com/srid/emanote";
        };
      };
    in
    lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
      # Load the container locally with: `nix build .#container && podman load < ./result`
      packages = { inherit container; };

      # Run this script in the CI to publish a new image
      apps = {
        publish-container-release.program = pkgs.writeShellApplication {
          name = "emanote-release";
          runtimeInputs = [ pkgs.crane ];
          text = ''
            set -e
            IMAGE="${container-name}"

            echo "Logging to registry..."
            printf '%s' "$GH_TOKEN" | crane auth login --username "$GH_USERNAME" --password-stdin ghcr.io

            echo "Publishing the image..."
            gunzip -c ${container} > image.tar || cp ${container} image.tar
            crane push image.tar "$IMAGE:${emanote.version}"

            echo "Tagging latest"
            crane tag "$IMAGE:${emanote.version}" latest

            echo "Cleaning up..."
            rm -f image.tar
          '';
        };
      };
    };
}
