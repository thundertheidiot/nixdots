{
  meow.wireguard = {
    enable = true;
    peer = "home";
    tcpPorts = [ 2284 ];
  };

  # Docker owns 2283; a dedicated native listener keeps outer FORWARD default-deny.
  services.nginx.virtualHosts.immich-transport = {
    listen = [
      {
        addr = "0.0.0.0";
        port = 2284;
      }
    ];
    locations."/" = {
      proxyPass = "http://127.0.0.1:2283";
      proxyWebsockets = true;
      extraConfig = "client_max_body_size 50000M;";
    };
  };
}
