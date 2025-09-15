{
  config,
  lib,
  mlib,
  ...
}: let
  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf;

  cfg = config.meow.server;
in {
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
      '';

      cert = "/var/lib/acme/${realm}/fullchain.pem";
      pkey = "/var/lib/acme/${realm}/key.pem";
    };

    users.users.turnserver.extraGroups = ["acme"];

    networking.firewall = {
      allowedUDPPortRanges = with config.services.coturn; [
        {
          from = min-port;
          to = max-port;
        }
      ];

      allowedUDPPorts = [3478 5349];
      allowedTCPPorts = [3478 5349];
    };
  };
}
