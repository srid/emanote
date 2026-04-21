# Build an Emanote static-site + live-server + link-check trio from a
# given `emanote` binary and a list of `layers`. Used internally for the
# docs site, and mirrors the semantics of the flake-parts `emanote.sites`
# module so third-party consumers stay on a familiar interface.
{ pkgs
, lib ? pkgs.lib
}:
{ name
, package
, layers
, basePath ? ""
, allowBrokenInternalLinks ? false
, extraConfig ? { }
, check ? true
, port ? 0
}:
let
  resolveLayer = l:
    let
      mountPoint = l.mountPoint or null;
      pathString = l.pathString or (builtins.toString l.path);
      suffix = lib.optionalString (mountPoint != null) "@${mountPoint}";
    in
    {
      layer = "${l.path}${suffix}";
      layerString = "${pathString}${suffix}";
    };
  resolved = map resolveLayer layers;

  configFile = (pkgs.formats.yaml { }).generate "emanote-index.yaml" extraConfig;
  configDir = pkgs.runCommand "emanote-deploy-layer" { } ''
    mkdir -p $out
    cp ${configFile} $out/index.yaml
  '';

  storeLayers = lib.concatStringsSep ";" (map (r: r.layer) resolved);
  runtimeLayers = lib.concatStringsSep ";" (map (r: r.layerString) resolved);

  staticSite = pkgs.runCommand "emanote-static-website-${name}"
    { meta.description = "Contents of the statically-generated Emanote website for ${name}"; }
    ''
      OUTPATH=$out/${basePath}
      mkdir -p $OUTPATH
      export LANG=C.UTF-8 LC_ALL=C.UTF-8  # https://github.com/srid/emanote/issues/125
      ${lib.getExe package} \
        --layers "${configDir};${storeLayers}" \
        ${lib.optionalString allowBrokenInternalLinks "--allow-broken-internal-links"} \
          gen $OUTPATH
    '';

  liveServer = pkgs.writeShellApplication {
    name = "emanote-with-layers-${name}";
    meta.description = "Live server for Emanote site ${name}";
    runtimeInputs = [ package ];
    text = ''
      set -xe
      if [ -z "$*" ]; then
        emanote --layers "${runtimeLayers}" \
          run ${lib.optionalString (port != 0) "--port ${toString port}"}
      else
        emanote --layers "${runtimeLayers}" \
          "$@"
      fi
    '';
  };

  linkCheck =
    if check then
      pkgs.runCommand "emanote-${name}-linkCheck"
        {
          buildInputs = [ pkgs.html-proofer ];
          meta.description = "Check links in the statically-generated Emanote website for ${name}";
        } ''
        export LANG=C.UTF-8 LC_ALL=C.UTF-8
        export RUBYOPT="-E utf-8"
        htmlproofer --disable-external ${staticSite}
        touch $out
      ''
    else null;
in
{
  package = staticSite;
  app = {
    type = "app";
    program = lib.getExe liveServer;
    meta = liveServer.meta;
  };
  check = linkCheck;
}
