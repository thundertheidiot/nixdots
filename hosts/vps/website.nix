{
  config,
  pkgs,
  ...
}: {
  meow.impermanence.directories = [
    {path = "/var/lib/kotiboksi";}
  ];

  meow.server.reverseProxy = {
    "${config.meow.server.mainDomain}" = "http://127.0.0.1:3005";
    "thunder.meowcloud.net" = "http://127.0.0.1:3005";
  };

  meow.server.radio.domains = ["thunder.meowcloud.net"];

  systemd.services."leptos-kotiboksi" = {
    enable = true;
    description = "Leptos Website";
    unitConfig = {
      Type = "simple";
    };

    environment = {
      DATABASE_URL = "/var/lib/kotiboksi/guestbook.db";
      LEPTOS_SITE_ADDR = "127.0.0.1:3005";
    };

    serviceConfig = {
      ExecStart = "${pkgs.leptos-kotiboksi}/bin/kotiboksi";
      WorkingDirectory = "/var/lib/kotiboksi";
      StateDirectory = "kotiboksi";
    };
    wantedBy = ["multi-user.target"];
  };
}
