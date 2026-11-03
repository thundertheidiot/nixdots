{
  meow.wireguard = {
    enable = true;
    peer = "home2";
    tcpPorts = [
      4533
      8096
      8428 # VictoriaMetrics remote write and queries from the VPS
      9100 # Node exporter scraped by the VPS
    ];
  };
}
