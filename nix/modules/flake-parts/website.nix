{ root, ... }:
{
  perSystem = { pkgs, lib, config, system, ... }: {
    emanote = {
      sites = {
        "docs" = {
          package = config.packages.default;
          layers = [{ path = (root + /docs); pathString = "./docs"; }];
          allowBrokenInternalLinks = true; # A couple, by design, in markdown.md
          allowBrokenLuaFilters = true; # `lua-filters/writing-filters.md` demos the error path
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
