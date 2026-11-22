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
    concatMapStrings
    filterAttrs
    findFirst
    mapAttrs
    mapAttrs'
    mapAttrsToList
    mkIf
    nameValuePair
    optionalAttrs
    ;
  inherit (lib.types) attrsOf str;

  cfg = config.meow.dns;
  inherit (cfg) domain;
  name = config.networking.hostName;

  # the network this host's dns clients sit on, wg reaches everything
  view = findFirst (net: infra.dns.resolvers.${net} == name) "wg" (attrNames infra.dns.resolvers);
  hosts = filterAttrs (_: host: host ? ${view} || host ? wg) infra.hosts;
  internal = filterAttrs (_: service: service ? internal) infra.services;

  allow =
    access:
    concatMapStrings (net: "allow ${net};\n") (
      {
        lan = [ infra.networks.lan ];
        private = [
          infra.networks.lan
          infra.networks.wg.cidr
        ];
        wg = [ infra.networks.wg.cidr ];
      }
      .${access}
      ++ [
        "127.0.0.1"
        "::1"
      ]
    )
    + "deny all;\n";
in
{
  options.meow.dns = {
    enable = mkEnOpt "internal dns records and wildcard certificate";
    domain = mkOpt str infra.dns.internal { };
    addresses = mkOpt (attrsOf str) (mapAttrs (_: host: host.${view} or host.wg) hosts) {
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

    sops.secrets.porkbun = {
      sopsFile = infra.secrets.porkbun;
      format = "dotenv";
    };

    security.acme.certs.${domain} = {
      domain = "*.${domain}";
      extraDomainNames = [ domain ];
      dnsProvider = "porkbun";
      # the local dnsmasq shadows the zone
      dnsResolver = "1.1.1.1:53";
      environmentFile = config.sops.secrets.porkbun.path;
    };

    services.nginx.virtualHosts = mapAttrs' (
      service:
      {
        access ? null,
        ...
      }:
      nameValuePair cfg.names.${service} (
        { useACMEHost = domain; } // optionalAttrs (access != null) { extraConfig = allow access; }
      )
    ) (filterAttrs (_: service: service.host == name) internal);
  };
}
