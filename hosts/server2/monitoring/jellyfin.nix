{
  config,
  lib,
  mlib,
  pkgs,
  ...
}:
let
  inherit (lib) getExe';
  inherit (mlib) applyWithJq;
  systemXml = "${config.services.jellyfin.configDir}/system.xml";
  xq = getExe' pkgs.yq "xq";
in
{
  config = {
    services.nginx.virtualHosts."jellyfin.local".locations."= /metrics".return = "404";

    # Metrics setup must never prevent Jellyfin itself from starting.
    systemd.services.jellyfin.preStart = ''
      if [ -f "${systemXml}" ] && [ "$(${xq} -r '.ServerConfiguration.EnableMetrics // "false"' < "${systemXml}")" != true ]; then
        if ${
          applyWithJq {
            jq = xq;
            args = "-x";
            operation = ''.ServerConfiguration.EnableMetrics = "true"'';
            file = systemXml;
          }
        } then
          :
        else
          echo "Unable to enable Jellyfin metrics in ${systemXml}" >&2
        fi
      fi
    '';
  };
}
