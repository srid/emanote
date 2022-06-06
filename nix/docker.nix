# Builds a docker image containing the emanote executable
#
# Run as:
#   docker load -i $(
#     nix-build nix/docker.nix \
#       --argstr name <image-name> \
#       --argstr tag <image-tag>
#   )
let
  pkgs =
    import
      (
        let
          lock = builtins.fromJSON (builtins.readFile ./flake.lock);
        in
        fetchTarball {
          url = "https://github.com/NixOS/nixpkgs/archive/${lock.nodes.nixpkgs.locked.rev}.tar.gz";
          sha256 = lock.nodes.nixpkgs.locked.narHash;
        }
      )
      { };
  emanote = (import ./.).packages.x86_64-linux.default;
in
{ name ? "sridca/emanote"
, tag ? "dev"
}: pkgs.dockerTools.buildImage {
  inherit name tag;
  contents = [
    emanote
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
    Cmd = [ "${emanote}/bin/emanote" ];
  };
}
