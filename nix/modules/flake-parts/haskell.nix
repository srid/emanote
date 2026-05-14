{ root, inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
  ];

  perSystem = { pkgs, lib, config, system, diagramEngineBins, diagramsTypstPackageRoot, ... }: {
    # haskell-flake configuration
    haskellProjects.default = {
      projectFlakeName = "emanote";

      # Whitelist to avoid unnecessary rebuilds of Nix package.
      projectRoot = builtins.toString (lib.fileset.toSource {
        inherit root;
        fileset = lib.fileset.unions [
          (root + /emanote)
          (root + /cabal.project)
        ];
      });

      devShell.tools = hp: {
        inherit (pkgs)
          stork
          tailwindcss_4;
      };
      autoWire = [ "packages" "apps" "checks" ];
      packages = {
        unionmount.source = inputs.unionmount;
        commonmark-simple.source = inputs.commonmark-simple;
        commonmark-wikilink.source = inputs.commonmark-wikilink;
        fsnotify.source = "0.4.1.0"; # Not in nixpkgs, yet.
        ghcid.source = "0.8.8";
        heist-extra.source = inputs.heist-extra;

        # MCP (Model Context Protocol) server library.
        # Pulled from GitHub because 0.3.1.0 isn't in the pinned
        # all-cabal-hashes snapshot and mcp-types isn't packaged in nixpkgs.
        mcp.source = inputs.dpella-mcp + /mcp-server;
        mcp-types.source = inputs.dpella-mcp + /mcp-types;
        # jsonrpc is an mcp-types dependency missing from nixpkgs.
        jsonrpc.source = inputs.dpella-jsonrpc;

        ema.source = inputs.ema + /ema;
        ema-generics.source = inputs.ema + /ema-generics;
        ema-extra.source = inputs.ema + /ema-extra;
        lvar.source = inputs.lvar;
      };

      settings = {
        # Haskell packages in nixpkgs are often broken in many ways; ergo,
        # it is our responsibility to fix them here.
        fsnotify.check = false;
        heist.broken = false;
        ixset-typed.broken = false;
        ixset-typed.jailbreak = true;
        pandoc-link-context.broken = false;
        pandoc-link-context.jailbreak = true;
        tagtree.broken = false;
        tagtree.jailbreak = true;
        unionmount.check = !pkgs.stdenv.isDarwin; # garnix: Slow M1 builder
        emanote = { name, pkgs, self, super, ... }:
          let
            # https://github.com/NixOS/cabal2nix/issues/608
            addMeta = pkg: pkg.overrideAttrs (oldAttrs: {
              meta = (oldAttrs.meta or { }) // {
                longDescription = ''
                  Emanote is a tool for generating a structured view of your
                  plain-text notes on the web, as a statically generated
                  website as well as a local live server.

                  For editing notes, you can use any text editor of your
                  choice including the likes of Obsidian.
                '';
              };
            });
            # Wrap the binary with the diagram-engine binaries and the
            # offline Typst package cache the bundled `diagram.lua`
            # filter needs. The engine set and package root both live
            # in `nix/modules/flake-parts/diagrams.nix` — adding a new
            # engine edits one file, not this one. `--set-default` lets
            # a user with their own typst-package cache override
            # Emanote's.
            wrapDiagramEngines = pkg: pkg.overrideAttrs (oldAttrs: {
              nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];
              postInstall = (oldAttrs.postInstall or "") + ''
                wrapProgram $out/bin/emanote \
                  --prefix PATH : ${lib.makeBinPath diagramEngineBins} \
                  --set-default TYPST_PACKAGE_PATH ${diagramsTypstPackageRoot}
              '';
            });
          in
          {
            check = false;
            extraBuildDepends = [ pkgs.stork pkgs.tailwindcss_4 ];
            custom = pkg: wrapDiagramEngines (addMeta pkg);
          };
      };
    };

    packages.default = config.packages.emanote;
    apps.default = config.apps.emanote;
  };
}
