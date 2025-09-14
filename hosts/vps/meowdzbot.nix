{
  config,
  pkgs,
  ...
}: {
  meow.impermanence.directories = [
    {path = "/var/lib/meowdzbot";}
  ];

  meow.server.reverseProxy = {
    "dz.${config.meow.server.mainDomain}" = "http://127.0.0.1:8080";
  };

  systemd.services."meowdzbot" = {
    enable = true;
    description = "meowdzbot";
    unitConfig = {
      Type = "simple";
    };

    environment = {
      DATABASE_URL = "/var/lib/meowdzbot/database.db";
    };

    serviceConfig = {
      ExecStart = "${pkgs.meowdzbot}/bin/meowdz-bot";
      EnvironmentFile = config.sops.secrets."meowdzbot_env".path;
      WorkingDirectory = "${pkgs.meowdzbot}";
      StateDirectory = "meowdzbot";
    };
    wantedBy = ["multi-user.target"];
  };
}
