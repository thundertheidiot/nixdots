{
  config,
  lib,
  inputs,
  ...
}:
let
  inherit (lib) concatMapStringsSep getExe';
  vpn = import "${inputs.self.outPath}/sops/wireguard";
in
{
  networking.firewall.extraCommands = ''
    ${getExe' config.networking.firewall.package "iptables"} -A nixos-fw \
      -i wg0 -s ${vpn.peers.${vpn.hub}.address} -d ${vpn.peers.home2.address} \
      -p tcp -m multiport --dports ${concatMapStringsSep "," toString config.meow.wireguard.tcpPorts} \
      -j nixos-fw-accept
  '';

  meow.wireguard = {
    enable = true;
    peer = "home2";
    tcpPorts = [
      4243 # comin exporter
      4533
      8096
      8428 # victoriametrics remote write and queries from the VPS
      9100 # node exporter scraped by the VPS
      9113 # nginx exporter
      9114 # forgejo metrics only
      9153 # dnsmasq exporter
      9187 # PostgreSQL exporter
    ];
  };
}
