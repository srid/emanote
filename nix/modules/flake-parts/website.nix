{ root, ... }:
{
  perSystem = { pkgs, lib, config, system, ... }: {
    emanote = {
      sites = {
        "docs" = {
          package = config.packages.default;
          layers = [{ path = (root + /docs); pathString = "./docs"; }];
          allowBrokenInternalLinks = true; # A couple, by design, in markdown.md
          extraConfig = {
            template = {
              urlStrategy = "pretty";
            };
          };
        };
      };
    };
  };
}
