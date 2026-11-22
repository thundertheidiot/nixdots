{
  config,
  infra,
  mlib,
  lib,
  ...
}:
let
  inherit (mlib) mkEnOpt mkOpt;
  inherit (lib)
    attrNames
    attrValues
    concatMap
    filter
    listToAttrs
    mapAttrs
    mkDefault
    mkIf
    ;
  inherit (lib.types) attrsOf str;

  cfg = config.meow.server;
  name = config.networking.hostName;

  # public names land on the edge, which proxies them to the service host over wg
  public = filter (s: s ? public) (attrValues infra.services);
  target =
    s:
    "http://${if s.host == name then "127.0.0.1" else infra.hosts.${s.host}.wg}:${toString s.port}";
in
{
  options.meow.server.webserver = mkEnOpt "Nginx";
  options.meow.server.reverseProxy = mkOpt (attrsOf str) { } { };

  config = mkIf cfg.webserver {
    networking.firewall.allowedTCPPorts = [
      80
      443
    ];

    meow.server.reverseProxy = mkIf (name == infra.dns.edge) (
      listToAttrs (
        concatMap (
          s:
          map (domain: {
            name = domain;
            value = target s;
          }) s.public
        ) (filter (s: s ? port) public)
      )
    );

    # names without a port are served by their own module
    meow.server.certificates =
      attrNames cfg.reverseProxy
      ++ (if name == infra.dns.edge then concatMap (s: s.public) public else [ ]);

    services.nginx = {
      enable = true;

      virtualHosts = mapAttrs (_: proxyPass: {
        locations."/" = {
          proxyPass = mkDefault proxyPass;
          recommendedProxySettings = mkDefault true;
        };
      }) cfg.reverseProxy;
    };
  };
}
