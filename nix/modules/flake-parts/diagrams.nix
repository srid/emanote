{
  # Shared Typst-package layout consumed by the bundled
  # `lua-filters/diagram.lua` filter. Exposed via `_module.args` so
  # both the wrapped binary (haskell.nix) and the devshell
  # (devshell.nix) can point Typst at the same offline package set.
  #
  # Typst resolves `@<namespace>/<name>:<version>` imports under
  # `$TYPST_PACKAGE_PATH/<namespace>/<name>/<version>/`. nixpkgs ships
  # each typst package at `lib/typst-packages/<name>/<version>/`, so
  # this derivation walks every transitive package and symlinks its
  # files into the `preview/<name>/<version>/` slot.
  #
  # The package set is enumerated manually because typst-packages
  # propagate via `propagatedBuildInputs` but we want a path-shaped
  # layout, not a build-input closure. cetz 0.3.4 imports
  # `@preview/oxifmt:0.2.1` from its `src/util.typ`; add new
  # transitive deps here when bumping cetz or introducing engines.
  perSystem = { pkgs, lib, ... }: {
    _module.args.diagramsTypstPackageRoot =
      let
        typstPackages = with pkgs.typstPackages; [
          cetz_0_3_4
          oxifmt_0_2_1
        ];
      in
      pkgs.runCommand "emanote-typst-packages" { } ''
        mkdir -p $out/preview
        ${lib.concatMapStringsSep "\n" (p: ''
          for entry in ${p}/lib/typst-packages/*/*; do
            name=$(basename "$(dirname "$entry")")
            version=$(basename "$entry")
            mkdir -p "$out/preview/$name"
            ln -s "$entry" "$out/preview/$name/$version"
          done
        '') typstPackages}
      '';
  };
}
