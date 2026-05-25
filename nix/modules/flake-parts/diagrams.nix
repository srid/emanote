{
  # Diagram-engine provisioning for the bundled `lua-filters/diagram.lua`
  # filter. Owns both axes that need to move when an engine is added or
  # bumped: the binary set on the wrapped emanote PATH and the offline
  # Typst package root. `checks.diagrams-typst-offline` compiles a real
  # cetz document against the root so a missing transitive dep fails
  # `nix flake check` instead of one note's render.
  perSystem = { pkgs, lib, ... }:
    let
      diagramsCetzVersion = "0.3.4";
      versionAttr = v: "cetz_" + lib.replaceStrings [ "." ] [ "_" ] v;
      # Single source of truth for "which engines does Emanote ship".
      # Consumed by `haskell.nix` (wraps the binary's PATH) and
      # `devshell.nix` (dev-shell tools); adding a new engine here puts
      # it in both places. Lua-side dispatch is separate (see
      # `default_engines` in `emanote/default/lua-filters/diagram.lua`).
      diagramEngineBins = [ pkgs.d2 pkgs.typst ];
      diagramsTypstPackageRoot =
        let
          # HACK: nixpkgs ships each typst package at
          # `lib/typst-packages/<name>/<version>/`, which Typst can't read
          # directly — its `TYPST_PACKAGE_PATH` resolver wants
          # `<root>/preview/<name>/<version>/`. The runCommand below
          # reshapes the layout via symlinks. A proper fix is in nixpkgs:
          # either expose a `typstPackages.makeOfflineRoot` helper, or
          # ship each package at the `preview/`-shaped path so consumers
          # can `symlinkJoin` directly.
          #
          # HACK: transitive `@preview/…` imports are listed manually
          # because typst-packages propagate via `propagatedBuildInputs`
          # but we need a path-shaped layout, not a build-input closure.
          # cetz 0.3.4 imports `@preview/oxifmt:0.2.1` from `src/util.typ`;
          # when bumping `diagramsCetzVersion`, grep the new release's
          # source for `@preview/...` and update this list. The proper
          # fix would be a closure walker that reads each package's
          # `typst.toml`/sources and recursively pulls every
          # `@preview/<name>:<ver>` it imports.
          typstPackages = [
            pkgs.typstPackages.${versionAttr diagramsCetzVersion}
            pkgs.typstPackages.oxifmt_0_2_1 # transitive dep of cetz 0.3.4
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
      # A self-contained shell that carries the diagram engines on PATH
      # plus the env vars the bundled `lua-filters/diagram.lua` reads
      # (`TYPST_PACKAGE_PATH`, `EMANOTE_CETZ_VERSION`). `devshell.nix`
      # picks this up via `inputsFrom`; `haskell.nix` reads the raw
      # values off `passthru` for `wrapProgram`. Adding a new engine
      # edits this file alone.
      devShells.diagrams = pkgs.mkShell {
        name = "emanote-diagrams";
        packages = diagramEngineBins;
        TYPST_PACKAGE_PATH = diagramsTypstPackageRoot;
        EMANOTE_CETZ_VERSION = diagramsCetzVersion;
        passthru = {
          inherit diagramsCetzVersion diagramEngineBins diagramsTypstPackageRoot;
        };
      };
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
