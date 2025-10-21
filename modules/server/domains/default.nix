{
  lib,
  mlib,
  config,
  pkgs,
  ...
}: let
  inherit (mlib) mkOpt mkEnOpt mkEnOptTrue;
  inherit (lib.types) attrsOf nullOr str;
  inherit (lib) mkIf mkMerge mapAttrsToList;

  cfg = config.meow.server.domains;
in {
  options.meow.server.domains = {
    enable = mkEnOpt "Enable local dns + certificates";
    openFirewall = mkEnOptTrue "Open firewall";
    interface = mkOpt (nullOr str) null {
      description = "Optionally bind to an interface";
    };
    domains = mkOpt (attrsOf str) {} {
      description = "Attribute set of domains and their assigned addresses";
    };
  };

  config = mkMerge [
    (mkIf cfg.enable
      && cfg.openFirewall {
        networking.firewall.allowedUDPPorts = [53];
        networking.firewall.allowedTCPPorts = [53];
      })
    (mkIf cfg.interface
      != null {
        services.dnsmasq.settings.interface = cfg.interface;
      })
    (mkIf cfg.interface
      == null {
        services.dnsmasq.settings.listen-address = "::,0.0.0.0";
      })
    (mkIf cfg.enable {
      services.dnsmasq = {
        enable = true;
        resolveLocalQueries = true;
        settings = {
          resolv-file = pkgs.writeText "resolv.conf" ''
            nameserver 1.1.1.1
          '';
          cache-size = 1000;
          address = mapAttrsToList (domain: addr: "/${domain}/addr") cfg.domains;
          server = [
            "1.1.1.1"
          ];
        };
      };
    })
  ];
}
