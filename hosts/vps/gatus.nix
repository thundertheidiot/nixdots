{
  server,
  config,
  ...
}: {
  config = {
    meow.server.reverseProxy = {
      "meowcloud.net" = "http://127.0.0.1:${toString config.services.gatus.settings.web.port}";
    };

    meow.impermanence.directories = [
      "/var/lib/private/gatus"
    ];

    services.gatus = {
      enable = true;
      settings = {
        web.port = 8081;

        storage = {
          type = "sqlite";
          path = "/var/lib/gatus/data.db";
        };

        ui = {
          header = "MeowCloud";
          title = "Health Dashboard";
          description = "Status page for MeowCloud services, powered by Gatus";
          dashboard-subheading = "Real-time service status";
        };

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
            name = "Jellyfin";
            group = "home";
            url = "http://${server.homeServer2}:8096";
            interval = "30s";
            conditions = [
              "[STATUS] == 200"
            ];
          }
          {
            name = "VaultWarden";
            group = "VPS";
            url = "http://127.0.0.1:${toString config.services.vaultwarden.config.ROCKET_PORT}/alive";
            interval = "30s";
            conditions = [
              "[STATUS] == 200"
            ];
          }
          {
            name = "Prosody (XMPP)";
            group = "VPS";
            url = "http://127.0.0.1:5280/health";
            interval = "30s";
            conditions = [
              "[STATUS] == 200"
            ];
          }
          {
            name = "Tuwunel (Matrix)";
            group = "VPS";
            url = "http://127.0.0.1:8008/_tuwunel/server_version";
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
