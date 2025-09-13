{
  config,
  inputs,
  ...
}: let
  root = inputs.self.outPath;
  keys = import "${root}/sops/wireguard";
in {
  imports = [keys.vps];

  config = {
    networking.wg-quick.interfaces.wg0 = {
      address = ["10.100.0.1/24"];
      listenPort = 51820;

      privateKeyFile = config.sops.secrets.wg_private.path;

      peers = [
        # Homeserver - port forward
        {
          allowedIPs = ["10.100.0.2/32"];
          publicKey = builtins.readFile keys.pubkeyHome;
          presharedKeyFile = config.sops.secrets.wg_home_psk.path;
        }
      ];
    };

    networking.firewall.allowedUDPPorts = [51820];

    boot.kernel.sysctl = {
      "net.ipv4.ip_forward" = 1;
      "net.ipv6.conf.all.forwarding" = 1;
    };
  };
}
