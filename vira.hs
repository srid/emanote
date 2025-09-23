-- CI configuration <https://vira.nixos.asia/>
\ctx pipeline ->
  let
    isMaster = ctx.branch == "master"
  in pipeline
     { signoff.enable = True
     , attic.enable = isMaster
     }
