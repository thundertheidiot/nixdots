{
  config,
  inputs,
  ...
}: let
  root = inputs.self.outPath;
  keys = import "${root}/sops/wireguard";
in {
  imports = [keys.home];

  config = {
    networking.wireguard.enable = true;
    networking.wireguard.interfaces.wg0 = {
      ips = ["10.100.0.2/24"];
      listenPort = 51820;

      privateKeyFile = config.sops.secrets.wg_private.path;

      peers = [
        {
          allowedIPs = ["10.100.0.0/24"];
          publicKey = builtins.readFile keys.pubkeyVps;
          presharedKeyFile = config.sops.secrets.wg_vps_psk.path;
          persistentKeepalive = 25;

          endpoint = "95.216.151.56:51820";
        }
      ];
    };
  };
}
