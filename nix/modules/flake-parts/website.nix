{
  perSystem = { pkgs, lib, config, system, ... }: {
    emanote = {
      package = config.packages.default;
      sites = {
        "docs" = {
          layers = [{ path = ./docs; pathString = "./docs"; }];
          allowBrokenLinks = true; # A couple, by design, in markdown.md
          prettyUrls = true;
        };
      };
    };
  };
}
