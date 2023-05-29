# Settings common to all local Haskell packages
{ lib, config, ... }: {
  settings =
    let
      localPackages = lib.filterAttrs (_: p: p.local) config.packages;
    in
    lib.mapAttrs
      (name: p: {
        haddock = false; # Because, this is end-user software. No need for library docs.
        libraryProfiling = false; # Avoid double-compilation.
        justStaticExecutables = p.cabal.executables != [ ]; # Reduce closure size
      })
      localPackages;
}
