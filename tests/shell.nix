# Nix shell for the cucumber/Playwright e2e suite.
#
# Provides Node.js and a Playwright-compatible Chromium so the suite
# runs on NixOS or any Nix-only host where
# `npx playwright install --with-deps` (which shells out to
# `sudo apt-get`) cannot work.
#
# The pin below is constrained by `playwright-driver` matching the
# `playwright` version in `tests/package.json` — diverging the two
# raises "browser revision X not found at <PLAYWRIGHT_BROWSERS_PATH>"
# at launch. Bump both together. Kept independent from the parent
# flake's nixpkgs because that pin is driven by the Haskell build.
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
