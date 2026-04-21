# Standalone shell providing chrome-devtools-mcp (Claude Code MCP server)
# with Chromium bundled, so `just mcp-chrome-devtools` works on machines
# without a system Chrome binary.
#
# Adapted from juspay/kolu#650: the upstream flake packages only the MCP
# server and defers Chrome provisioning to the consumer. Emanote is the
# consumer here, so Chromium is pinned and wired into --executable-path.
#
# nixpkgs is pulled straight from the top-level flake.lock so this shell
# stays consistent with the rest of the build without a second pin to
# maintain.
let
  lock = builtins.fromJSON (builtins.readFile ../../flake.lock);
  nixpkgsLocked = lock.nodes.nixpkgs.locked;
  pkgs = import
    (builtins.fetchTree {
      type = "github";
      owner = nixpkgsLocked.owner;
      repo = nixpkgsLocked.repo;
      rev = nixpkgsLocked.rev;
      narHash = nixpkgsLocked.narHash;
    })
    { };
  mcpVersion = "0.21.0";
  mcp = pkgs.writeShellScriptBin "chrome-devtools-mcp" ''
    exec ${pkgs.nodejs}/bin/npx -y chrome-devtools-mcp@${mcpVersion} \
      --executable-path=${pkgs.chromium}/bin/chromium "$@"
  '';
in
pkgs.mkShell {
  packages = [ mcp ];
}
