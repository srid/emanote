{
  perSystem = { pkgs, lib, config, system, diagramsTypstPackageRoot, ... }: {
    devShells.default =
      lib.addMetaAttrs { description = "Emanote development environment"; }
        (pkgs.mkShell {
          name = "emanote-dev";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.pre-commit.devShell
          ];
          packages = with pkgs; [
            just
          ];
          # Mirror the wrapped binary's typst package cache (see
          # diagrams.nix) so `cabal run` from this devshell resolves
          # `@preview/cetz` offline too.
          TYPST_PACKAGE_PATH = diagramsTypstPackageRoot;
        });
  };
}
