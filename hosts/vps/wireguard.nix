{
  config,
  inputs,
  pkgs,
  ...
}: let
  root = inputs.self.outPath;
  keys = import "${root}/sops/wireguard";
in {
  imports = [keys.vps.module];

  config = {
    networking.wg-quick.interfaces.wg0 = {
      address = ["10.100.0.1/24"];
      privateKeyFile = config.sops.secrets.wg_private.path;

      listenPort = 51820;

      peers = [
        # Homeserver - port forward
        {
          allowedIPs = ["10.100.0.2/32"];
          publicKey = keys.home.pubkey;
          presharedKeyFile = config.sops.secrets.wg_preshared.path;
        }
        # Homeserver 2 - port forward
        {
          allowedIPs = ["10.100.0.3/32"];
          publicKey = keys.home2.pubkey;
          presharedKeyFile = config.sops.secrets.wg_preshared.path;
        }
      ];
    };

    networking.firewall.allowedUDPPorts = [51820];

    boot.kernel.sysctl = {
      "net.ipv4.ip_forward" = 1;
      "net.ipv6.conf.all.forwarding" = 1;
    };

    # proxy ssh from home
    services.nginx.package = pkgs.nginxMainline;
    services.nginx.streamConfig = ''
      upstream home_ssh {
        server 10.100.0.2:22;
      }

      upstream home2_ssh {
        server 10.100.0.3:22;
      }

      server {
        listen 2222;
        proxy_pass home2_ssh;
      }
    '';

    networking.firewall.allowedTCPPorts = [2222];
  };
}
