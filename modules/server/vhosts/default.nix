{
  config,
  infra,
  lib,
  ...
}:
let
  inherit (lib)
    attrValues
    concatLists
    concatMap
    concatMapStrings
    filterAttrs
    listToAttrs
    mapAttrs'
    mapAttrsToList
    mkIf
    mkMerge
    nameValuePair
    optionalAttrs
    ;

  hostName = config.networking.hostName;
  inherit (config.meow.dns) names;
  inherit (config.meow.server.cluster) services;

  # the vhost config shared by every name of a service, internal or public
  # upstreamAddress is where the service listens, as seen from this host
  mkServiceVhost =
    service: upstreamAddress:
    optionalAttrs (service.port != null) (
      service.proxy "http://${upstreamAddress}:${toString service.port}"
    );
in
{
  config = mkMerge [
    (
      let
        localServices = filterAttrs (
          _: service: service.internal != null && service.host == hostName
        ) services;

        networksFor =
          access:
          {
            private = [
              infra.networks.lan
              infra.networks.wg.cidr
            ];
            wg = [ infra.networks.wg.cidr ];
          }
          .${access};

        # nginx acl, loopback is always let in
        mkAccessRules =
          access:
          concatMapStrings (network: "allow ${network};\n") (
            networksFor access
            ++ [
              "127.0.0.1"
              "::1"
            ]
          )
          + "deny all;\n";
      in
      mkIf config.meow.dns.enable {
        services.nginx.virtualHosts = mapAttrs' (
          serviceName: service:
          nameValuePair names.${serviceName} (mkMerge [
            (mkServiceVhost service "127.0.0.1")
            {
              forceSSL = true;
              useACMEHost = infra.dns.internal;
            }
            (optionalAttrs (service.access != null) { extraConfig = mkAccessRules service.access; })
          ])
        ) localServices;

        meow.kanidm.protect = mapAttrs' (
          serviceName: service: nameValuePair names.${serviceName} service.protect
        ) (filterAttrs (_: service: service.protect != [ ]) localServices);
      }
    )

    (
      let
        # public names land on the edge, which proxies them to the service host over wg
        publicServices = filterAttrs (_: service: service.expose != [ ]) services;

        # one attribute per public domain, valueFor gets the service the domain belongs to
        perPublicDomain =
          services: valueFor:
          listToAttrs (
            concatLists (
              mapAttrsToList (
                _: service: map (domain: nameValuePair domain (valueFor service)) service.expose
              ) services
            )
          );

        upstreamAddress =
          service: if service.host == hostName then "127.0.0.1" else infra.hosts.${service.host}.wg;
      in
      mkIf (hostName == infra.dns.edge && config.meow.server.webserver) {
        # names without a port are served by their own module
        meow.server.certificates = concatMap (service: service.expose) (attrValues publicServices);

        services.nginx.virtualHosts = perPublicDomain publicServices (
          service: mkServiceVhost service (upstreamAddress service)
        );

        meow.kanidm.protect = perPublicDomain (filterAttrs (
          _: service: service.protect != [ ]
        ) publicServices) (service: service.protect);
      }
    )
  ];
}
