{ root, inputs, self, ... }:
{
  flake = {
    homeManagerModule = { lib, pkgs, ... }: {
      imports = [
        (root + /nix/modules/home/emanote.nix)
      ];
      services.emanote.package = lib.mkDefault self.packages.${pkgs.stdenv.hostPlatform.system}.default;
    };
    flakeModule = (root + /nix/modules/flake-parts/flake-module);
    templates.default = {
      description = "A simple flake.nix template for emanote notebooks";
      path = builtins.path { path = inputs.emanote-template; filter = path: _: baseNameOf path == "flake.nix"; };
    };
  };
}
