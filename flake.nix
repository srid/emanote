# Zero flake inputs by design: flake input resolution on `nix develop`
# eval is expensive. All pins live in `npins/sources.json` and are loaded
# through `nix/nixpkgs.nix`, which default.nix and shell.nix reuse for
# the non-flake path.
{
  description = "emanote: Emanate a structured view of your plain-text notes";
  nixConfig = {
    extra-substituters = "https://cache.nixos.asia/oss";
    extra-trusted-public-keys = "oss:KO872wNJkCDgmGN3xy9dT89WAhvv13EiKncTtHDItVU=";
  };

  outputs = { self }:
    let
      sources = import ./npins;
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];

      # haskell-flake's `evalHaskellProject` is the expensive step; share
      # it across default.nix (packages/apps/checks) and shell.nix (devShell).
      perSystem = pkgs:
        let
          project = import ./nix/haskell-project.nix { inherit pkgs; };
          outs = import ./default.nix { inherit pkgs project; };
        in
        {
          packages = {
            default = outs.default;
            emanote = outs.emanote;
            docs = outs.docs;
          };
          apps = {
            default = {
              type = "app";
              program = pkgs.lib.getExe outs.emanote;
              meta.description = "Emanote live server";
            };
            docs = outs.docs-app;
          };
          devShells.default = import ./shell.nix { inherit pkgs project; };
          checks = {
            closure-size = outs.closure-size;
          } // pkgs.lib.optionalAttrs (outs.docs-linkCheck != null) {
            docs-linkCheck = outs.docs-linkCheck;
          };
        };

      perSystemOutputs = builtins.listToAttrs (map
        (system: {
          name = system;
          value = perSystem (import ./nix/nixpkgs.nix { inherit system; });
        })
        systems);

      project = attr: builtins.mapAttrs (_: s: s.${attr}) perSystemOutputs;
    in
    {
      homeManagerModules.default = import ./nix/home/emanote.nix;
      homeManagerModule = self.homeManagerModules.default; # legacy alias

      flakeModule = ./nix/flake-module;

      templates.default = {
        description = "A simple flake.nix template for emanote notebooks";
        path = builtins.path {
          path = sources.emanote-template;
          filter = path: _: baseNameOf path == "flake.nix";
        };
      };

      packages = project "packages";
      apps = project "apps";
      devShells = project "devShells";
      checks = project "checks";
    };
}
