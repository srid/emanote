{ config, lib, pkgs, ... }:
let

  cfg = config.services.emanote;

  yamlFormat = pkgs.formats.yaml { } // {
    generateDir = name: path: value:
      (yamlFormat.generate path value).overrideAttrs (attrs: {
        inherit name;
        buildCommand = ''
          mkdir -p $out
          out="$out/${path}"
          ${attrs.buildCommand}
        '';
      });
  };

  extraConfig = lib.recursiveUpdate cfg.extraConfig {
    template.baseUrl = cfg.baseUrl;
  };
  configLayer = yamlFormat.generateDir "emanote-config" "/index.yaml" extraConfig;
  layers = map toString ([ configLayer ] ++ cfg.notes);

in
{
  options = {
    services.emanote = with lib; {
      enable = mkEnableOption ''
        Enable the Emanote service, to create beautiful websites --
        such as your personal webpage, blog, wiki, Zettelkasten,
        notebook, knowledge-base, documentation, etc. from
        future-proof plain-text notes and arbitrary data -- with a
        live preview that updates in real-time.
      '';

      package = mkOption {
        type = types.package;
        default = pkgs.emanote;
        defaultText = "pkgs.emanote";
        description = "Emanote derivation to use";
      };

      notes = mkOption {
        type = types.nonEmptyListOf (types.either types.path types.str);
        default = [ "${config.home.homeDirectory}/Documents/Notes" ];
        defaultText = literalExample ''[ ''${config.home.homeDirectory}/Documents/Notes" ]'';
        description = ''
          List of notebook folders to 'union mount', with earlier
          paths in the list taking priority over later paths.
        '';
      };

      host = mkOption {
        type = types.str;
        default = "127.0.0.1";
        description = "The hostname or IP address the HTTP server should listen to";
      };

      port = mkOption {
        type = types.port;
        default = 7000;
        description = "The port on which the HTTP server listens.";
      };

      baseUrl = mkOption {
        type = types.str;
        description = "Base href for hyperlinks on the site.";
        default = "/";
        example = "/emanote/";
      };

      extraConfig = mkOption {
        inherit (yamlFormat) type;
        default = { };
        example = {
          template.urlStrategy = "pretty";
        };
        description = ''
          Config that will be layered over the default notes configs.

          See https://emanote.srid.ca/demo/yaml-config for
          information about the format.
        '';
      };

      systemdTarget = mkOption {
        type = types.str;
        default = "graphical-session.target";
        description = ''
          The systemd user unit which
          <literal>emanote.service<literal> should be a part of.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    systemd.user.services.emanote = {
      Unit = {
        Description = "Emanote web server";
        PartOf = [ cfg.systemdTarget ];
        After = [ cfg.systemdTarget ];
      };
      Install.WantedBy = [ cfg.systemdTarget ];
      Service = {
        ExecStart = ''
          ${cfg.package}/bin/emanote \
            --layers "${lib.concatStringsSep ";" layers}" \
            run --host=${cfg.host} --port=${builtins.toString cfg.port}
        '';
      };
    };
  };
}
