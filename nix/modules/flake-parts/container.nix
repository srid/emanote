# TODO: Make a flake-parts module out of this, for publish container images built in Nix to ghcr.io
{ root, ... }: {
  perSystem = { pkgs, config, lib, system, ... }:
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

      # Push single-arch image (called from GitHub CI matrix)
      apps = {
        publish-container-arch.program = pkgs.writeShellApplication {
          name = "emanote-release-arch";
          runtimeInputs = [ pkgs.crane pkgs.file ];
          text = ''
            set -e
            IMAGE="${container-name}"
            SYSTEM="${system}"
            
            # Map current system to container architecture
            case "$SYSTEM" in
              x86_64-linux)
                ARCH="amd64"
                ;;
              aarch64-linux)
                ARCH="arm64"
                ;;
              *)
                echo "Unsupported system: $SYSTEM"
                exit 1
                ;;
            esac
            
            echo "Using pre-built container for $SYSTEM ($ARCH)..."
            CONTAINER_PATH="${container}"
            
            echo "Logging to registry..."
            printf '%s' "$GH_TOKEN" | crane auth login --username "$GH_USERNAME" --password-stdin ghcr.io

            echo "Publishing $ARCH image..."
            # Check if the container output is gzipped and handle accordingly
            if file -L "$CONTAINER_PATH" | grep -q "gzip compressed"; then
              echo "Container is gzipped, decompressing to temporary location..."
              TMPDIR=$(mktemp -d)
              trap 'rm -rf $TMPDIR' EXIT
              gunzip -c "$CONTAINER_PATH" > "$TMPDIR/image.tar"
              echo "Pushing $TMPDIR/image.tar to $IMAGE:${emanote.version}-$ARCH"
              crane push "$TMPDIR/image.tar" "$IMAGE:${emanote.version}-$ARCH"
            else
              echo "Container is already a tar file, pushing directly..."
              echo "Pushing $CONTAINER_PATH to $IMAGE:${emanote.version}-$ARCH"
              crane push "$CONTAINER_PATH" "$IMAGE:${emanote.version}-$ARCH"
            fi

            # Verify the image was pushed successfully
            echo "Verifying image was pushed successfully..."
            if crane manifest "$IMAGE:${emanote.version}-$ARCH" >/dev/null 2>&1; then
              echo "$ARCH image pushed and verified successfully!"
            else
              echo "ERROR: Failed to verify $ARCH image was pushed"
              exit 1
            fi
          '';
        };

        # Create and push multi-arch manifest (run from single job after arch builds)
        publish-container-manifest.program = pkgs.writeShellApplication {
          name = "emanote-manifest";
          runtimeInputs = [ pkgs.docker pkgs.crane ];
          text = ''
            set -e
            IMAGE="${container-name}"
            VERSION="${emanote.version}"
            
            echo "Logging to registry..."
            echo "$GH_TOKEN" | docker login ghcr.io -u "$GH_USERNAME" --password-stdin

            # Verify both architecture images exist before creating manifest
            echo "Verifying required images exist..."
            if ! crane manifest "$IMAGE:$VERSION-amd64" >/dev/null 2>&1; then
              echo "ERROR: AMD64 image $IMAGE:$VERSION-amd64 not found"
              exit 1
            fi
            if ! crane manifest "$IMAGE:$VERSION-arm64" >/dev/null 2>&1; then
              echo "ERROR: ARM64 image $IMAGE:$VERSION-arm64 not found"
              exit 1
            fi
            echo "Both architecture images verified successfully"

            echo "Creating multi-arch manifest for version $VERSION..."
            docker buildx imagetools create \
              -t "$IMAGE:$VERSION" \
              "$IMAGE:$VERSION-amd64" \
              "$IMAGE:$VERSION-arm64"

            echo "Creating multi-arch manifest for latest..."
            docker buildx imagetools create \
              -t "$IMAGE:latest" \
              "$IMAGE:$VERSION-amd64" \
              "$IMAGE:$VERSION-arm64"

            echo "Multi-arch manifest created successfully!"
          '';
        };
      };
    };
}
