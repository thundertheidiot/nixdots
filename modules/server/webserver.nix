{
  config,
  mlib,
  lib,
  ...
}: let
  inherit (mlib) mkEnOpt mkOpt;
  inherit (lib) mkIf attrNames mapAttrs;
  inherit (lib.types) attrsOf str;

  cfg = config.meow.server;
in {
  options.meow.server.webserver = mkEnOpt "Nginx";
  options.meow.server.reverseProxy = mkOpt (attrsOf str) {} {};

  config = mkIf cfg.webserver {
    networking.firewall.allowedTCPPorts = [80 443];

    meow.server.certificates = attrNames cfg.reverseProxy;

    services.nginx = {
      enable = true;
      logError = "stderr debug";

      virtualHosts =
        mapAttrs (_: proxyPass: {
          locations."/" = {
            inherit proxyPass;
            recommendedProxySettings = true;
          };
        })
        cfg.reverseProxy;
    };
  };
}
