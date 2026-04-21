# Standalone shell providing chrome-devtools-mcp (Claude Code MCP server)
# with Chromium bundled, so `just mcp-chrome-devtools` works on machines
# without a system Chrome binary.
#
# Adapted from juspay/kolu#650: the upstream flake packages only the MCP
# server and defers Chrome provisioning to the consumer. Emanote is the
# consumer here, so Chromium is pinned and wired into --executable-path.
{ pkgs ? import
    (fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz";
    })
    { }
}:
let
  mcpVersion = "0.21.0";
  mcp = pkgs.writeShellScriptBin "chrome-devtools-mcp" ''
    exec ${pkgs.nodejs}/bin/npx -y chrome-devtools-mcp@${mcpVersion} \
      --executable-path=${pkgs.chromium}/bin/chromium "$@"
  '';
in
pkgs.mkShell {
  packages = [ mcp ];
}
