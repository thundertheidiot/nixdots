{ infra, ... }:
{
  meow.wireguard.enable = true;

  # internal vhosts and kanidm replication
  networking.firewall.interfaces.wg0.allowedTCPPorts = [
    443
    infra.kanidm.replicationPort
  ];
}
