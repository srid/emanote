# Nix shell for the cucumber/Playwright e2e suite.
#
# Provides Node.js and a Playwright-compatible Chromium so the suite
# runs on NixOS or any Nix-only host where
# `npx playwright install --with-deps` (which shells out to
# `sudo apt-get`) cannot work.
#
# nixpkgs is read from the parent flake's `nixpkgs-latest` input —
# kept independent from the main `nixpkgs` input (which the Haskell
# build pins) so the Playwright pin can move on its own cadence. The
# constraint: this rev's `playwright-driver` version must match the
# `playwright` version in `tests/package.json`. Diverging the two
# raises "browser revision X not found at <PLAYWRIGHT_BROWSERS_PATH>"
# at launch — bump both together via `nix flake update nixpkgs-latest`
# plus a matching npm bump.
let
  lock = builtins.fromJSON (builtins.readFile ../flake.lock);
  nixpkgsLocked = lock.nodes.nixpkgs-latest.locked;
  pkgs = import
    (builtins.fetchTree {
      type = "github";
      owner = nixpkgsLocked.owner;
      repo = nixpkgsLocked.repo;
      rev = nixpkgsLocked.rev;
      narHash = nixpkgsLocked.narHash;
    })
    { };
in
pkgs.mkShell {
  packages = [ pkgs.nodejs ];
  PLAYWRIGHT_BROWSERS_PATH = "${pkgs.playwright-driver.browsers}";
  PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "1";
}
