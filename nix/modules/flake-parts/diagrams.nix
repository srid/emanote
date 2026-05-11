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
  # `@preview/oxifmt:0.2.1` from its `src/util.typ`; bumping the cetz
  # version below means grepping the new release's source for
  # `@preview/...` imports and adding any new transitive deps here.
  # The `checks.diagrams-typst-offline` derivation below catches a
  # missed dep at evaluation time by compiling a real cetz document
  # against this package root — `nix flake check` fails loudly if
  # the closure is incomplete.
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
      # Compile a minimal cetz document against the offline package
      # root. A missing transitive `@preview/…` import surfaces here
      # rather than at note-render time as a `Pandoc Lua filter error`
      # banner.
      checks.diagrams-typst-offline = pkgs.runCommand "diagrams-typst-offline"
        {
          nativeBuildInputs = [ pkgs.typst ];
          TYPST_PACKAGE_PATH = diagramsTypstPackageRoot;
        } ''
        cat > smoke.typ <<EOF
        #import "@preview/cetz:${diagramsCetzVersion}"
        #set page(width: auto, height: auto, margin: 0.5cm)
        #cetz.canvas({
          import cetz.draw: *
          circle((0, 0), radius: 1)
        })
        EOF
        typst compile -f svg smoke.typ "$out"
      '';
    };
}
