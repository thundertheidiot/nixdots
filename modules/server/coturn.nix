{
  config,
  lib,
  mlib,
  ...
}:
let
  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf;

  cfg = config.meow.server;
in
{
  options.meow.server.coturn = mkEnOpt "Coturn";

  config = mkIf cfg.coturn {
    services.coturn = rec {
      enable = true;
      use-auth-secret = true;
      static-auth-secret-file = config.sops.secrets.coturn_secret.path;

      min-port = 49000;
      max-port = 50000;

      no-cli = true;
      no-tcp-relay = true;
      realm = cfg.mainDomain;

      extraConfig = ''
        verbose
        no-multicast-peers
        stale-nonce
      ''
      # prevent coturn from reaching any internal addresses
      + ''
        denied-peer-ip=0.0.0.0-0.255.255.255
        denied-peer-ip=10.0.0.0-10.255.255.255
        denied-peer-ip=100.64.0.0-100.127.255.255
        denied-peer-ip=127.0.0.0-127.255.255.255
        denied-peer-ip=169.254.0.0-169.254.255.255
        denied-peer-ip=172.16.0.0-172.31.255.255
        denied-peer-ip=192.0.0.0-192.0.0.255
        denied-peer-ip=192.0.2.0-192.0.2.255
        denied-peer-ip=192.168.0.0-192.168.255.255
        denied-peer-ip=198.18.0.0-198.19.255.255
        denied-peer-ip=198.51.100.0-198.51.100.255
        denied-peer-ip=203.0.113.0-203.0.113.255
        denied-peer-ip=224.0.0.0-255.255.255.255
        denied-peer-ip=95.216.151.56
        denied-peer-ip=::1-::1
        denied-peer-ip=fc00::-fdff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
        denied-peer-ip=fe80::-febf:ffff:ffff:ffff:ffff:ffff:ffff:ffff
      '';

      cert = "/var/lib/acme/${realm}/fullchain.pem";
      pkey = "/var/lib/acme/${realm}/key.pem";
    };

    users.users.turnserver.extraGroups = [ "acme" ];

    networking.firewall = {
      allowedUDPPortRanges = with config.services.coturn; [
        {
          from = min-port;
          to = max-port;
        }
      ];

      allowedUDPPorts = [
        3478
        5349
      ];
      allowedTCPPorts = [
        3478
        5349
      ];
    };
  };
}
