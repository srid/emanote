# TODO: Make a flake-parts module out of this, for publish container images built in Nix to ghcr.io
{ root, ... }: {
  perSystem = { pkgs, config, lib, system, ... }:
    let
      emanote = config.packages.emanote;
      container-name = "ghcr.io/srid/emanote-dev";

      # Helper to get architecture string for container tagging
      archFromSystem = sys:
        if sys == "x86_64-linux" then "amd64"
        else if sys == "aarch64-linux" then "arm64"
        else throw "Unsupported system: ${sys}";

      currentArch = archFromSystem system;

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

      # Push single-arch image (called from CI matrix)
      apps = {
        publish-container-arch.program = pkgs.writeShellApplication {
          name = "emanote-release-arch";
          runtimeInputs = [ pkgs.crane ];
          text = ''
            set -e
            IMAGE="${container-name}"
            ARCH="${currentArch}"
            
            echo "Logging to registry..."
            printf '%s' "$GH_TOKEN" | crane auth login --username "$GH_USERNAME" --password-stdin ghcr.io

            echo "Publishing $ARCH image..."
            gunzip -c ${container} > image.tar || cp ${container} image.tar
            crane push image.tar "$IMAGE:${emanote.version}-$ARCH"

            echo "Cleaning up..."
            rm -f image.tar
          '';
        };

        # Create and push multi-arch manifest (run from single job after arch builds)
        publish-container-manifest.program = pkgs.writeShellApplication {
          name = "emanote-manifest";
          runtimeInputs = [ pkgs.crane ];
          text = ''
            set -e
            IMAGE="${container-name}"
            VERSION="${emanote.version}"
            
            echo "Logging to registry..."
            printf '%s' "$GH_TOKEN" | crane auth login --username "$GH_USERNAME" --password-stdin ghcr.io

            echo "Creating multi-arch manifest for version $VERSION..."
            crane index append \
              --tag "$IMAGE:$VERSION-amd64" \
              --tag "$IMAGE:$VERSION-arm64" \
              "$IMAGE:$VERSION"

            echo "Creating multi-arch manifest for latest..."
            crane index append \
              --tag "$IMAGE:$VERSION-amd64" \
              --tag "$IMAGE:$VERSION-arm64" \
              "$IMAGE:latest"

            echo "Multi-arch manifest created successfully!"
          '';
        };
      };
    };
}
