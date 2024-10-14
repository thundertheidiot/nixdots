{config, ...}: {
  config = {
    networking.firewall.allowedTCPPorts = [80];

    services.nginx = {
      enable = true;
      logError = "stderr debug";
    };
  };
}
