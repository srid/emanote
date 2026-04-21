final: prev: {
  # Stork is marked broken on x86_64-darwin in nixpkgs, but it does work.
  # We lack CI for intel Mac (#335), so trust the upstream binary.
  stork =
    if prev.stdenv.hostPlatform.system == "x86_64-darwin"
    then prev.stork.overrideAttrs (_: { meta.broken = false; })
    else prev.stork;
}
