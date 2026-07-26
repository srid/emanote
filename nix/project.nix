{ diagrams, pkgs, root, sources, withDevShell ? true }:
let
  inherit (pkgs) lib;
  projectRoot = builtins.toString (lib.fileset.toSource {
    inherit root;
    fileset = lib.fileset.unions [
      (root + /emanote)
      (root + /cabal.project)
    ];
  });
in
(import "${sources.haskell-flake}/nix/lib.nix" { inherit pkgs; }).evalHaskellProject {
  inherit projectRoot;
  modules = [{
    devShell =
      if withDevShell then {
        tools = _hp: {
          inherit (pkgs) stork tailwindcss_4;
        };
      } else {
        enable = false;
      };
    packages = {
      unionmount.source = sources.unionmount;
      commonmark-simple.source = sources.commonmark-simple;
      commonmark-wikilink.source = sources.commonmark-wikilink;
      fsnotify.source = "0.4.1.0";
      ghcid.source = "0.8.8";
      heist-extra.source = sources.heist-extra;
      mcp.source = sources.dpella-mcp + /mcp-server;
      mcp-types.source = sources.dpella-mcp + /mcp-types;
      jsonrpc.source = sources.dpella-jsonrpc;
      ema.source = sources.ema + /ema;
      ema-generics.source = sources.ema + /ema-generics;
      ema-extra.source = sources.ema + /ema-extra;
      lvar.source = sources.lvar;
    };
    settings = {
      fsnotify.check = false;
      heist.broken = false;
      ixset-typed = {
        broken = false;
        jailbreak = true;
      };
      pandoc-link-context = {
        broken = false;
        jailbreak = true;
      };
      tagtree = {
        broken = false;
        jailbreak = true;
      };
      unionmount.check = !pkgs.stdenv.isDarwin;
      emanote = { ... }:
        let
          addMeta = package: package.overrideAttrs (old: {
            meta = (old.meta or { }) // {
              longDescription = ''
                Emanote is a tool for generating a structured view of your
                plain-text notes on the web, as a statically generated
                website as well as a local live server.

                For editing notes, you can use any text editor of your
                choice including the likes of Obsidian.
              '';
            };
          });
          wrapDiagramEngines = package: package.overrideAttrs (old: {
            nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];
            postInstall = (old.postInstall or "") + ''
              wrapProgram $out/bin/emanote \
                --prefix PATH : ${lib.makeBinPath diagrams.diagramEngineBins} \
                --set-default TYPST_PACKAGE_PATH ${diagrams.diagramsTypstPackageRoot} \
                --set-default EMANOTE_CETZ_VERSION ${diagrams.diagramsCetzVersion}
            '';
          });
        in
        {
          check = false;
          extraBuildDepends = [ pkgs.stork pkgs.tailwindcss_4 ];
          custom = package: wrapDiagramEngines (addMeta package);
        };
    };
  }];
}
