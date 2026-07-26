{ pkgs }:
let
  inherit (pkgs) lib;
  diagramsCetzVersion = "0.3.4";
  versionAttr = version: "cetz_" + lib.replaceStrings [ "." ] [ "_" ] version;
  diagramEngineBins = [ pkgs.d2 pkgs.typst ];
  diagramsTypstPackageRoot =
    let
      # nixpkgs stores each Typst package below lib/typst-packages, while
      # TYPST_PACKAGE_PATH expects <root>/preview/<name>/<version>.
      typstPackages = [
        pkgs.typstPackages.${versionAttr diagramsCetzVersion}
        pkgs.typstPackages.oxifmt_0_2_1
      ];
    in
    pkgs.runCommand "emanote-typst-packages" { } ''
      mkdir -p $out/preview
      ${lib.concatMapStringsSep "\n" (package: ''
        for entry in ${package}/lib/typst-packages/*/*; do
          name=$(basename "$(dirname "$entry")")
          version=$(basename "$entry")
          mkdir -p "$out/preview/$name"
          ln -s "$entry" "$out/preview/$name/$version"
        done
      '') typstPackages}
    '';
  devShell = pkgs.mkShell {
    name = "emanote-diagrams";
    packages = diagramEngineBins;
    TYPST_PACKAGE_PATH = diagramsTypstPackageRoot;
    EMANOTE_CETZ_VERSION = diagramsCetzVersion;
  };
  check = pkgs.runCommand "diagrams-typst-offline"
    {
      nativeBuildInputs = [ pkgs.typst ];
      TYPST_PACKAGE_PATH = diagramsTypstPackageRoot;
    }
    ''
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
in
{
  inherit check devShell diagramEngineBins diagramsCetzVersion diagramsTypstPackageRoot;
}
