{
  perSystem = { pkgs, lib, config, system, diagramEngineBins, diagramsTypstPackageRoot, ... }: {
    devShells.default =
      lib.addMetaAttrs { description = "Emanote development environment"; }
        (pkgs.mkShell {
          name = "emanote-dev";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.pre-commit.devShell
          ];
          # Diagram engines explicitly listed here — same set wraps the
          # installed binary's PATH in `haskell.nix`; both sites read
          # `diagramEngineBins` from `diagrams.nix`, so adding a new
          # engine touches one file.
          packages = [ pkgs.just ] ++ diagramEngineBins;
          # Mirror the wrapped binary's typst package cache (see
          # diagrams.nix) so `cabal run` from this devshell resolves
          # `@preview/cetz` offline too.
          TYPST_PACKAGE_PATH = diagramsTypstPackageRoot;
        });
  };
}
