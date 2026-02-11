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
            group = "home";
            url = "icmp://${server.homeServer2}";
            interval = "30s";
            conditions = [
              "[CONNECTED] == true"
            ];
          }
          {
            name = "Prosody";
            group = "VPS";
            url = "http://127.0.0.1:5280/health";
            interval = "30s";
            conditions = [
              "[STATUS] == 200"
            ];
          }
          {
            name = "MeowDZBot";
            group = "VPS";
            url = "http://127.0.0.1:8080/health";
            interval = "30s";
            conditions = [
              "[STATUS] == 200"
            ];
          }
        ];
      };
    };
  };
}
