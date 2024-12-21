{config, ...}: {
  meow.sops.enableSecrets = [
    "server_slskd_env"
  ];

  meow.impermanence.ensureDirectories = [
    "/persist/media/downloads/incomplete/soulseek"
    "/persist/media/downloads/soulseek"
  ];

  vpnNamespaces."airvpn" = {
    portMappings = [
      {
        from = config.services.slskd.settings.web.port;
        to = 5030;
        protocol = "tcp";
      }
    ];

    openVPNPorts = [
      {
        port = config.services.slskd.settings.soulseek.listen_port;
        protocol = "both";
      }
    ];
  };

  systemd.services.slskd.vpnConfinement = {
    enable = true;
    vpnNamespace = "airvpn";
  };

  users.users.slskd.uid = 3001;
  users.groups.slskd.gid = 3001;

  services.slskd = {
    enable = true;
    environmentFile = "/run/secrets/server_slskd_env";

    domain = null;
    settings = {
      shares.directories = [
        "[Music]/persist/media/music"
        "[Metal Music]/persist/media/metal"
      ];

      directories = {
        downloads = "/persist/media/downloads/soulseek";
        incomplete = "/persist/media/downloads/incomplete/soulseek";
      };

      web = {
        port = 5030;
        https.disabled = true;
        authentication.api_keys.soularr.key = "soularrapikeysoularr";
        authentication.api_keys.soularr.cidr = "127.0.0.1";
      };

      soulseek.listen_port = 33493;

      global = {
        # Slow but my home internet is not very fast :(
        upload.speed_limit = 1000;
      };
    };
  };
}
