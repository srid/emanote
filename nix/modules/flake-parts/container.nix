# TODO: Make a flake-parts module out of this, for publish container images built in Nix to ghcr.io
{ root, inputs, ... }: {
  perSystem = { pkgs, config, lib, system, ... }:
    let
      emanote = config.packages.emanote;
      container-name = "ghcr.io/srid/emanote";
      nix2containerPkgs = inputs.nix2container.packages.${system};

      # Create a policy.json file for skopeo in the Nix store
      skopeoPolicy = pkgs.writeText "policy.json" (builtins.toJSON {
        default = [{
          type = "insecureAcceptAnything";
        }];
      });

      container = nix2containerPkgs.nix2container.buildImage {
        name = container-name;
        tag = "latest";
        copyToRoot = [ emanote ];
        config = {
          Entrypoint = [ "${emanote}/bin/emanote" ];
          WorkingDir = "/notebook";
          Env = [ "LANG=C.UTF-8" ];
          Labels = {
            "org.opencontainers.image.source" = "https://github.com/srid/emanote";
          };
        };
      };
    in
    lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
      # Load the container locally with: `nix build .#container && skopeo copy nix2container:./result docker-daemon:emanote:latest`
      # or import into docker with: `nix build .#container && skopeo copy nix2container:./result docker://registry.local/emanote && docker pull registry.local/emanote`
      packages = { inherit container; };

      # Push single-arch image (called from CI matrix)
      apps = {
        publish-container-arch.program = pkgs.writeShellApplication {
          name = "emanote-release-arch";
          runtimeInputs = [ nix2containerPkgs.skopeo-nix2container pkgs.nix ];
          text = ''
            set -euo pipefail
            IMAGE="${container-name}"
            TARGET_SYSTEM="$1"  # Pass target system as argument (e.g., x86_64-linux, aarch64-linux)
            
            # Map system to container architecture
            case "$TARGET_SYSTEM" in
              x86_64-linux)
                ARCH="amd64"
                ;;
              aarch64-linux)
                ARCH="arm64"
                ;;
              *)
                echo "Unsupported system: $TARGET_SYSTEM"
                exit 1
                ;;
            esac
            
            echo "Building container for $TARGET_SYSTEM ($ARCH)..."
            CONTAINER_PATH=$(nix build --no-link --print-out-paths --system "$TARGET_SYSTEM" .#container)
            
            echo "Publishing $ARCH image..."
            echo "Container manifest at: $CONTAINER_PATH"
            echo "Using skopeo-nix2container version: $(skopeo --version)"
            
            # Use skopeo-nix2container to copy from the nix2container output to the registry
            echo "Running: skopeo --insecure-policy copy nix2container:$CONTAINER_PATH docker://$IMAGE:${emanote.version}-$ARCH"
            skopeo --insecure-policy copy \
              "nix2container:$CONTAINER_PATH" \
              "docker://$IMAGE:${emanote.version}-$ARCH" \
              --dest-creds="$GH_USERNAME:$GH_TOKEN"

            echo "$ARCH image pushed successfully!"
          '';
        };

        # Create and push multi-arch manifest (run from single job after arch builds)
        publish-container-manifest.program = pkgs.writeShellApplication {
          name = "emanote-manifest";
          runtimeInputs = [ pkgs.docker ];
          text = ''
            set -e
            IMAGE="${container-name}"
            VERSION="${emanote.version}"
            
            echo "Logging to registry..."
            echo "$GH_TOKEN" | docker login ghcr.io -u "$GH_USERNAME" --password-stdin

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
