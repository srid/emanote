# Standalone Nix shell for cucumber/Playwright e2e suites.
#
# Provides Node.js and a Playwright-compatible Chromium so the suite
# runs on NixOS or any Nix-only host where
# `npx playwright install --with-deps` (which shells out to
# `sudo apt-get`) cannot work.
#
# Consumers:
#   nix-shell tests/nix/shell.nix --run '<cmd>'
# or via the companion just module (`tests/nix/mod.just`):
#   mod e2e 'tests/nix/mod.just'
#   just e2e run "<cmd>"
#
# The pin below is constrained by `playwright-driver` matching the npm
# `playwright` version your tests import — diverging the two raises
# "browser revision X not found at <PLAYWRIGHT_BROWSERS_PATH>" at
# launch. Bump both together. This rev tracks Kolu so the two repos
# stay aligned ahead of an eventual extract-to-shared-repo move.
let
  nixpkgs = builtins.fetchTree {
    type = "github";
    owner = "nixos";
    repo = "nixpkgs";
    rev = "f8573b9c935cfaa162dd62cc9e75ae2db86f85df";
    narHash = "sha256-hpXH0z3K9xv0fHaje136KY872VT2T5uwxtezlAskQgY=";
  };
  pkgs = import nixpkgs { };
in
pkgs.mkShell {
  packages = [ pkgs.nodejs ];
  PLAYWRIGHT_BROWSERS_PATH = "${pkgs.playwright-driver.browsers}";
  PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "1";
}
