-- Pipeline configuration for Vira
\ctx pipeline ->
  let isMaster = ctx.branch == "master"
  in pipeline
    { signoff.enable = True
    , cache.url = if isMaster then Just "https://cache.nixos.asia/oss" else Nothing
    }
