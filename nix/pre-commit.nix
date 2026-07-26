{ pkgs, root, sources }:
let
  fourmolu = (import "${sources.fourmolu-nix}/lib").mkWrapper pkgs {
    settings = {
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
  tools = import "${sources.git-hooks}/nix/call-tools.nix" pkgs;
  run = pkgs.callPackage "${sources.git-hooks}/nix/run.nix" {
    inherit pkgs tools;
    isFlakes = true;
    gitignore-nix-src = { };
  };
  check = run {
    src = root;
    hooks = {
      nixpkgs-fmt = {
        enable = true;
        excludes = [ "npins/default\\.nix" ];
      };
      fourmolu = {
        enable = true;
        package = fourmolu;
        excludes = [ "vira\\.hs" ];
      };
      hlint = {
        enable = true;
        excludes = [ "vira\\.h" ];
      };
      cabal-fmt.enable = true;
    };
  };
  devShell = pkgs.mkShell {
    inherit (check) shellHook;
    packages = check.enabledPackages;
  };
in
{
  inherit check devShell;
}
