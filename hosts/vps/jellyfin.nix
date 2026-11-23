{ config, infra, ... }:
let
  inherit (config.meow.server.cluster.services) jellyfin;
in
{
  config = {
    meow.server.gatus = [
      {
        name = "Jellyfin";
        group = "home";
        url = "http://${infra.hosts.${jellyfin.host}.wg}:${toString jellyfin.port}";
        interval = "30s";
        conditions = [
          "[STATUS] == 200"
        ];
      }
    ];
  };
}
