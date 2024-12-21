{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./torrent.nix
    ./slskd.nix
    ./arr.nix
    ./soularr.nix
    ./virtualhosts.nix
  ];

  meow.sops.enableSecrets = [
    "server_wireguard"
  ];

  vpnNamespaces."airvpn" = {
    enable = true;
    wireguardConfigFile = config.sops.secrets."server_wireguard".path;
    accessibleFrom = [
      "127.0.0.1"
      "192.168.100.101"
      # "192.168.10.0/24"
    ];
    namespaceAddress = "192.168.15.1";
    bridgeAddress = "192.168.15.5";
  };
}
