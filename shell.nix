# TODO: Remove this after https://github.com/srid/haskell-template/issues/23

(import
  (
    fetchTarball {
      url = "https://github.com/edolstra/flake-compat/archive/b4a34015c698c7793d592d66adbab377907a2be8.tar.gz";
      sha256 = "sha256-Z+s0J8/r907g149rllvwhb4pKi8Wam5ij0st8PwAh+E=";
    }
  )
  {
    src = ./.;
  }).shellNix
