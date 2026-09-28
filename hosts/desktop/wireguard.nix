{
  config,
  inputs,
  ...
}:
let
  keys = import "${inputs.self.outPath}/sops/wireguard";
in
{
  imports = [ keys.desktop.module ];

  config = {
    networking.wg-quick.interfaces.wg0 = {
      address = [ "10.100.0.5/24" ];
      privateKeyFile = config.sops.secrets.wg_private.path;

      peers = [
        {
          allowedIPs = [ "10.100.0.0/24" ];
          publicKey = keys.vps.pubkey;
          presharedKeyFile = config.sops.secrets.wg_preshared.path;
          persistentKeepalive = 25;

          endpoint = "95.216.151.56:51820";
        }
      ];
    };
  };
}
