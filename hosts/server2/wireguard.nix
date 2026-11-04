{
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
