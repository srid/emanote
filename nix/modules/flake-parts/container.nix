{ root, ... }: {
  perSystem = { pkgs, config, lib, ... }:
    let
      emanote = config.packages.emanote;
      container-name = "ghcr.io/srid/emanote";
      container = pkgs.dockerTools.streamLayeredImage {
        name = container-name;
        tag = "latest";
        created = "now";
        config.Entrypoint = [ "${emanote}/bin/emanote" ];
        config.WorkingDir = "/site";
        config.Labels = {
          "org.opencontainers.image.source" = "https://github.com/srid/emanote";
        };
      };
    in {
      # Load the container locally with: `nix build .#container && ./result | podman load`
      packages = lib.optionalAttrs pkgs.stdenv.isLinux { container = container };

      # Run this script in the CI to publish a new image
      apps = lib.optionalAttrs pkgs.stdenv.isLinux { 
        publish-container-release = pkgs.writeShellScriptBin "emanote-release" ''
          set -e
          export PATH=$PATH:${pkgs.gzip}/bin:${pkgs.skopeo}/bin
          IMAGE="docker://${container-name}"

          echo "Logging to registry..."
          echo $GH_TOKEN | skopeo login --username $GH_USERNAME --password-stdin ghcr.io

          echo "Building and publishing the image..."
          ${container} | gzip --fast | skopeo copy docker-archive:/dev/stdin $IMAGE:${emanote.version}

          echo "Tagging latest"
          skopeo copy $IMAGE:${emanote.version} $IMAGE:latest
        '';
      };
    };
}
