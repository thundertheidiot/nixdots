{config, ...}: {
  meow.impermanence.directories = [
    {
      path = "/var/lib/immich";
      persistPath = "/nix/persist/immich";
      user = "immich";
      group = "immich";
    }
    {
      path = "/var/lib/postgresql";
      persistPath = "/nix/persist/postgresql";
    }
  ];

  server.domains = ["immich.local"];

  services.nginx.virtualHosts = {
    "immich.local" = {
      root = "/fake";
      locations = {
        "/" = {
          proxyPass = "http://127.0.0.1:${toString config.services.immich.port}";
          recommendedProxySettings = true;
        };
      };
    };
  };

  services.immich = {
    enable = true;
    redis.enable = true;
    settings = {
      server.externalDomain = "https://img.kotiboksi.xyz";
    };
    database = {
      enable = true;
      createDB = true;
    };
  };
}
