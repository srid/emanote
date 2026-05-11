{
  # Shared Typst-package layout consumed by the bundled
  # `lua-filters/diagram.lua` filter. Exposed via `_module.args` so
  # both the wrapped binary (haskell.nix) and the devshell
  # (devshell.nix) can point Typst at the same offline package set,
  # and the same cetz version reaches the filter's `@preview/cetz:…`
  # preamble via `EMANOTE_CETZ_VERSION` — one source of truth.
  #
  # Typst resolves `@<namespace>/<name>:<version>` imports under
  # `$TYPST_PACKAGE_PATH/<namespace>/<name>/<version>/`. nixpkgs ships
  # each typst package at `lib/typst-packages/<name>/<version>/`, so
  # this derivation walks every transitive package and symlinks its
  # files into the `preview/<name>/<version>/` slot.
  #
  # Transitive packages are enumerated manually because typst-packages
  # propagate via `propagatedBuildInputs` but we want a path-shaped
  # layout, not a build-input closure. cetz 0.3.4 imports
  # `@preview/oxifmt:0.2.1` from its `src/util.typ`; add new
  # transitive deps here when bumping the cetz version below.
  perSystem = { pkgs, lib, ... }:
    let
      diagramsCetzVersion = "0.3.4";
      versionAttr = v: "cetz_" + lib.replaceStrings [ "." ] [ "_" ] v;
      diagramsTypstPackageRoot =
        let
          typstPackages = [
            pkgs.typstPackages.${versionAttr diagramsCetzVersion}
            pkgs.typstPackages.oxifmt_0_2_1
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
    in
    {
      _module.args = { inherit diagramsCetzVersion diagramsTypstPackageRoot; };
    };
}
