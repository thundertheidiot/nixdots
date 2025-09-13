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
    networking.wg-quick.interfaces.wg0 = {
      address = ["10.100.0.2/24"];
      privateKeyFile = config.sops.secrets.wg_private.path;

      peers = [
        {
          allowedIPs = ["10.100.0.1/32"];
          publicKey = builtins.readFile keys.pubkeyVps;
          presharedKeyFile = config.sops.secrets.wg_vps_psk.path;
          persistentKeepalive = 25;

          endpoint = "95.216.151.56:51820";
        }
      ];
    };
  };
}
