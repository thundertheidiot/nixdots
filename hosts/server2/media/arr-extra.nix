{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    genAttrs
    ;
  settings = pkgs.writeText "arr-extra.json" (builtins.toJSON (import ./arr-extra-settings.nix));
  apps = [
    "radarr"
    "sonarr"
  ];
in
{
  config = {
    # An offline indexer must not prevent deploying the rest of the server.
    # nixflix still logs each rejected save; early API/transport failures remain fatal.
    systemd.services = {
      prowlarr-indexers.serviceConfig.SuccessExitStatus = [ 1 ];
    }
    // (genAttrs (map (app: "${app}-exported-settings") apps) (
      name:
      let
        app = lib.removeSuffix "-exported-settings" name;
        cfg = config.nixflix.${app}.config;
        dependencies = [ "${app}-config.service" ];
      in
      {
        description = "Apply ${app} download path mapping";
        after = dependencies;
        requires = dependencies;
        wantedBy = [ "multi-user.target" ];
        restartTriggers = [
          settings
          ./configure-arr.py
        ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          LoadCredential = [ "api-key:/run/media-stack/secrets/${app}-api-key" ];
          ExecStart = "${pkgs.python3}/bin/python3 ${./configure-arr.py} ${settings} ${app} http://${cfg.hostConfig.bindAddress}:${toString cfg.hostConfig.port}${cfg.hostConfig.urlBase}/api/${cfg.apiVersion} %d/api-key";
        };
      }
    ));
  };
}
