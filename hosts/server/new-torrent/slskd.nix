{
  config,
  pkgs,
  ...
}: {
  networking.firewall.interfaces."airvpn-sweden".allowedTCPPorts = [
    config.services.slskd.settings.soulseek.listen_port
  ];

  systemd.services.slskd.after = [
    "wg-quick-airvpn-sweden.service"
  ];

  users.users.slskd.uid = 3000;

  services.slskd = {
    enable = true;
    environmentFile = "/run/secrets/server_slskd_env";

    domain = null;
    settings = {
      shares.directories = [
        "[Music]/media/music"
        "[Metal Music]/media/metal"
      ];

      directories = {
        downloads = "/downloads/soulseek";
        incomplete = "/downloads/incomplete/soulseek";
      };

      web = {
        port = 5030;
        https.disabled = true;
      };

      soulseek.listen_port = 50300;

      global = {
        # Slow but my home internet is not very fast :(
        upload.speed_limit = 1000;
      };
    };
  };
}
