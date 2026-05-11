{
  # Offline Typst package root for the bundled `lua-filters/diagram.lua`
  # filter. Transitive `@preview/…` imports are listed explicitly
  # because the `lib/typst-packages/<name>/<version>/` layout nixpkgs
  # ships needs reshaping into the `<root>/preview/<name>/<version>/`
  # layout `TYPST_PACKAGE_PATH` expects, and that walk can't follow
  # propagated build inputs. `checks.diagrams-typst-offline` compiles
  # a real cetz document against this root so a missing transitive dep
  # fails `nix flake check` instead of one note's render.
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
      _module.args = { inherit diagramsTypstPackageRoot; };
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
