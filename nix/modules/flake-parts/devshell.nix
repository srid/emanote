{
  perSystem = { pkgs, lib, config, system, ... }: {
    devShells.default =
      lib.addMetaAttrs { description = "Emanote development environment"; }
        (pkgs.mkShell {
          name = "emanote-dev";
          # `devShells.diagrams` (defined in `diagrams.nix`) carries the
          # engine binaries on PATH plus `TYPST_PACKAGE_PATH` /
          # `EMANOTE_CETZ_VERSION`, so `cabal run` from this shell resolves
          # `@preview/cetz` offline identically to the wrapped binary.
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.pre-commit.devShell
            config.devShells.diagrams
          ];
          packages = [ pkgs.just ];
        });
  };
}
