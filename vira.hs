-- Pipeline configuration for Vira
\ctx pipeline ->
  let isMaster = ctx.branch == "master"
  in pipeline
    { build.systems = [ "x86_64-linux", "aarch64-darwin" ]
    , build.flakes =
        [ "."
        , "./nix/home-manager-test" { overrideInputs = [ ("emanote", ".") ] }
        ]
    , signoff.enable = True
    , cache.url = if isMaster then Just "https://cache.nixos.asia/oss" else Nothing
    }
