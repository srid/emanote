{
  perSystem = { pkgs, lib, config, system, ... }: {
   devShells.default =
      lib.addMetaAttrs { description = "Emanote development environment"; }
        (pkgs.mkShell {
          name = "emanote-dev";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.treefmt.build.devShell
          ];
          packages = with pkgs; [
            just
          ];
        });
 };
}
