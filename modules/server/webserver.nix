{
  config,
  mlib,
  lib,
  ...
}: let
  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf;

  cfg = config.meow.server;
in {
  options.meow.server.webserver = mkEnOpt "Nginx";

  config = mkIf cfg.webserver {
    networking.firewall.allowedTCPPorts = [80 443];

    services.nginx = {
      enable = true;
      logError = "stderr debug";
    };
  };
}
