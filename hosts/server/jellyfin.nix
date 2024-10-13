{config, ...}: {
  config = {
    services.jellyfin = {
      enable = true;
      openFirewall = true;
      cacheDir = "${config.services.jellyfin.dataDir}/cache";
    };

    meow.impermanence.directories = [
      {
        path = "/var/lib/jellyfin";
        persistPath = "${config.meow.impermanence.persist}/jellyfin";
        user = "jellyfin";
        group = "jellyfin";
      }
    ];
  };
}
