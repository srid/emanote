{ inputs, ... }:
{
  imports = [
    (inputs.git-hooks + /flake-module.nix)
    inputs.fourmolu-nix.flakeModule
  ];
  perSystem = { config, ... }: {
    pre-commit.settings = {
      hooks = {
        nixpkgs-fmt.enable = true;
        fourmolu = {
          enable = true;
          package = config.fourmolu.wrapper;
          excludes = [ "vira\.hs" ];
        };
        hlint = {
          enable = true;
          excludes = [ "vira\.h" ];
        };
        cabal-fmt.enable = true;
      };
    };

    fourmolu.settings = {
      indentation = 2;
      comma-style = "leading";
      record-brace-space = true;
      indent-wheres = true;
      import-export-style = "diff-friendly";
      respectful = true;
      haddock-style = "multi-line";
      newlines-between-decls = 1;
      extensions = [ "ImportQualifiedPost" ];
    };
  };
}
