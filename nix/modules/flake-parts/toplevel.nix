{ root, inputs, ... }:
{
  flake = {
    homeManagerModule = import (root + /nix/modules/home/emanote.nix);
    flakeModule = (root + /nix/modules/flake-parts/flake-module);
    templates.default = {
      description = "A simple flake.nix template for emanote notebooks";
      path = builtins.path { path = inputs.emanote-template; filter = path: _: baseNameOf path == "flake.nix"; };
    };
  };
}
