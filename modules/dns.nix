{
  config,
  infra,
  lib,
  mlib,
  ...
}:
let
  inherit (mlib) mkEnOpt mkOpt;
  inherit (lib)
    attrNames
    filterAttrs
    findFirst
    mapAttrs
    mapAttrsToList
    mkIf
    ;
  inherit (lib.types) attrsOf str;

  cfg = config.meow.dns;
  domain = infra.dns.internal;
  name = config.networking.hostName;

  # the network this host's dns clients sit on, wg reaches everything
  view = findFirst (net: infra.dns.resolvers.${net} == name) "wg" (attrNames infra.dns.resolvers);
  internal = filterAttrs (_: service: service.internal != null) config.meow.server.cluster.services;
in
{
  options.meow.dns = {
    enable = mkEnOpt "internal dns records and wildcard certificate";
    addresses = mkOpt (attrsOf str) (mapAttrs (_: host: host.${view} or host.wg) infra.hosts) {
      description = "host addresses as seen by this host's dns clients";
    };
    names = mkOpt (attrsOf str) (mapAttrs (_: service: "${service.internal}.${domain}") internal) {
      description = "internal name of each service";
    };
  };

  config = mkIf cfg.enable {
    services.dnsmasq.settings = {
      local = [ "/${domain}/" ];
      host-record =
        mapAttrsToList (service: { host, ... }: "${cfg.names.${service}},${cfg.addresses.${host}}") internal
        ++ mapAttrsToList (
          host: address: "${host}.${infra.dns.devices}.${domain},${address}"
        ) cfg.addresses;
    };

    meow.server.dnsCertificates = [ domain ];
    security.acme.certs.${domain} = {
      domain = "*.${domain}";
      extraDomainNames = [ domain ];
    };
  };
}
