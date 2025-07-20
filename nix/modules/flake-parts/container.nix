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
        config.WorkingDir = "/site";
        config.Labels = {
          "org.opencontainers.image.source" = "https://github.com/srid/emanote";
        };
      };
    in
    lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
      # Load the container locally with: `nix build .#container && podman load < ./result`
      packages = { container = container; };

      # Run this script in the CI to publish a new image
      apps = {
        publish-container-release.program = pkgs.writeShellScriptBin "emanote-release" ''
          set -e
          export PATH=$PATH:${pkgs.crane}/bin
          IMAGE="${container-name}"

          echo "Logging to registry..."
          echo $GH_TOKEN | crane auth login --username $GH_USERNAME --password-stdin ghcr.io

          echo "Building container image..."
          nix build .#container -o container-result

          echo "Publishing the image..."
          gunzip -c ./container-result > image.tar || cp ./container-result image.tar
          crane push image.tar $IMAGE:${emanote.version}

          echo "Tagging latest"
          crane tag $IMAGE:${emanote.version} latest

          echo "Cleaning up..."
          rm -f image.tar container-result
        '';
      };
    };
}
