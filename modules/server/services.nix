{
  config,
  infra,
  lib,
  nodes,
  ...
}:
let
  inherit (lib)
    all
    allUnique
    attrNames
    attrValues
    concatLists
    concatMap
    count
    elem
    filter
    findFirst
    genAttrs
    mapAttrsToList
    mergeAttrsList
    mkIf
    mkOption
    ;
  inherit (lib.types)
    attrs
    attrsOf
    enum
    functionTo
    listOf
    nullOr
    port
    raw
    str
    submodule
    ;

  cfg = config.meow.server;
  hostName = config.networking.hostName;

  # the ugly insides of this setup
  # the cluster hosts need to be read through an unchecked copy
  # checked config forces mkIf conditions, which goes into infinite recursion
  unchecked =
    host: (nodes.${host}.extendModules { modules = [ { _module.check = false; } ]; }).config;
  servers = genAttrs infra.serviceHosts (
    host: if host == hostName then cfg else (unchecked host).meow.server
  );
in
{
  options.meow.server = {
    services =
      let
        plainProxy = target: {
          locations."/" = {
            proxyPass = target;
            recommendedProxySettings = true;
          };
          # exporters are scraped over wg, never through the proxy
          locations."~* ^/metrics(/|$)".return = "404";
        };
      in
      mkOption {
        description = "services served by this host";
        default = { };
        type = attrsOf (
          submodule (
            { name, ... }:
            {
              options = {
                internal = mkOption {
                  description = "name under the internal zone, null for none";
                  type = nullOr str;
                  default = name;
                };
                access = mkOption {
                  description = "networks let in on the internal name, null for any";
                  type = nullOr (enum [
                    "private"
                    "wg"
                  ]);
                  default = "private";
                };
                protect = mkOption {
                  description = "kanidm groups let in through the local oauth2-proxy";
                  type = listOf str;
                  default = [ ];
                };
                port = mkOption {
                  description = "proxied by nginx, null when the service sets up its own vhost";
                  type = nullOr port;
                  default = null;
                };
                expose = mkOption {
                  description = "public names proxied by the edge over wg, the first one is canonical";
                  type = listOf str;
                  default = [ ];
                };
                host = mkOption {
                  description = "host the service runs on";
                  type = str;
                  default = hostName;
                };
                proxy = mkOption {
                  description = "vhost config for an upstream url";
                  type = functionTo attrs;
                  default = plainProxy;
                };
              };
            }
          )
        );
      };

    monitoring = {
      exporters = mkOption {
        description = "metrics scraped by the collector, over wg from other hosts";
        default = { };
        type = attrsOf (
          submodule (
            { name, ... }:
            {
              options = {
                port = mkOption { type = port; };
                job = mkOption {
                  type = str;
                  default = name;
                };
                labels = mkOption {
                  type = attrsOf str;
                  default = { };
                };
              };
            }
          )
        );
      };
      storage = mkOption {
        description = "long term metrics storage, remote written by the collector";
        type = nullOr port;
        default = null;
      };
    };

    cluster = {
      services = mkOption {
        description = "services of every service host";
        type = raw;
        default = mergeAttrsList (mapAttrsToList (_: server: server.services) servers);
      };
      exporters = mkOption {
        description = "exporters of every service host";
        type = raw;
        default = concatLists (
          mapAttrsToList (
            host: server: mapAttrsToList (name: e: e // { inherit host name; }) server.monitoring.exporters
          ) servers
        );
      };
      storage = mkOption {
        description = "the host and port of the metrics storage";
        type = raw;
        default = findFirst (storage: storage != null) null (
          mapAttrsToList (
            host: server:
            if server.monitoring.storage == null then
              null
            else
              {
                inherit host;
                port = server.monitoring.storage;
              }
          ) servers
        );
      };
    };
  };

  config = mkIf (elem hostName infra.serviceHosts) {
    assertions =
      let
        defined = concatMap (server: attrValues server.services) (attrValues servers);
        services = attrValues cfg.cluster.services;
        internal = filter (service: service.internal != null) services;
      in
      [
        {
          assertion = allUnique (concatMap (server: attrNames server.services) (attrValues servers));
          message = "service names must be unique across hosts";
        }
        {
          assertion = allUnique (map (service: service.internal) internal);
          message = "internal names must be unique";
        }
        {
          assertion = allUnique (concatMap (service: service.expose) defined);
          message = "public names must be unique";
        }
        {
          assertion = all (service: service.expose != [ ] -> infra.hosts.${service.host} ? wg) services;
          message = "exposed services must be reachable over wg";
        }
        {
          assertion = all (service: service.protect != [ ] -> service.internal != null) services;
          message = "protect needs an internal name";
        }
        {
          assertion = count (server: server.monitoring.storage != null) (attrValues servers) <= 1;
          message = "only one host can store metrics";
        }
      ];
  };
}
