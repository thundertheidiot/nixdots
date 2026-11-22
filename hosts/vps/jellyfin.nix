{ infra, ... }:
let
  jellyfin = infra.services.jellyfin;
in
{
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
}
