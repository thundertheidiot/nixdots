{
  server,
  config,
  ...
}: {
  config = {
    meow.server.reverseProxy = {
      "meowcloud.net" = "http://127.0.0.1:${toString config.services.gatus.settings.web.port}";
    };

    services.gatus = {
      enable = true;
      settings = {
        web.port = 8081;
        endpoints = [
          {
            name = "homeserver";
            url = "icmp://${server.homeServer2}";
            interval = "30s";
            conditions = [
              "[CONNECTED] == true"
            ];
          }
        ];
      };
    };
  };
}
