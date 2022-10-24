{ ... }:

{
  perSystem = { system, pkgs, ... }: {
    packages.tailwind =
      # TODO: We probably should export this from tailwind-haskell flake
      pkgs.nodePackages.tailwindcss.overrideAttrs (oa: {
        plugins = [
          pkgs.nodePackages."@tailwindcss/aspect-ratio"
          pkgs.nodePackages."@tailwindcss/forms"
          pkgs.nodePackages."@tailwindcss/language-server"
          pkgs.nodePackages."@tailwindcss/line-clamp"
          pkgs.nodePackages."@tailwindcss/typography"
        ];
      });
  };
}
