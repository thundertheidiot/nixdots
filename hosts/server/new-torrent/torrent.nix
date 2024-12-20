{
  pkgs,
  config,
  lib,
  ...
}: {
  networking.firewall.interfaces."airvpn-sweden".allowedUDPPorts = [
    config.services.transmission.settings.peer-port
  ];

  networking.firewall.interfaces."airvpn-sweden".allowedTCPPorts = [
    config.services.transmission.settings.peer-port
  ];

  systemd.services.transmission.requires = [
    "wg-quick-airvpn-sweden.service"
  ];

  systemd.services.transmission.serviceConfig = {
    # TODO check transmission systemd unit settings
    BindPaths =
      lib.mkForce [
      ];
    BindReadOnlyPaths = lib.mkForce [builtins.storeDir "/etc"];
  };

  services.transmission = {
    enable = true;
    webHome = "${pkgs.flood-for-transmission}";
    settings = {
      download-dir = "/downloads/torrents";
      incomplete-dir-enabled = true;
      incomplete-dir = "/downloads/incomplete/torrents";
      peer-port = 49948; # AirVPN forwarded port

      rpc-bind-address = "0.0.0.0";
      rpc-username = "admin";
      rpc-password = "admin";
      rpc-whitelist-enabled = false;
    };
  };
}
