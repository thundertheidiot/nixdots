{ lib }:
let
  inherit (builtins)
    all
    attrNames
    attrValues
    filter
    fromJSON
    hasAttr
    mapAttrs
    readFile
    ;
  inherit (lib)
    allUnique
    assertMsg
    optionalAttrs
    recursiveUpdate
    ;

  infra = {
    # sops files shared between hosts
    secrets = {
      porkbun = ./porkbun.env;
      kanidm = ./kanidm.yaml;
    };

    hosts = {
      vps = {
        public = {
          v4 = "95.216.151.56";
          v6 = "2a01:4f9:c010:9973::1";
        };
        wg = "10.100.0.1";
      };
      server2 = {
        lan = "192.168.101.111";
        wg = "10.100.0.3";
      };
      desktop = {
        lan = "192.168.101.100";
        wg = "10.100.0.5";
      };
      framework.wg = "10.100.0.4";
      phone.wg = "10.100.0.6";
      uwu.wg = "10.100.0.2";
    };

    networks = {
      lan = "192.168.101.0/24";
      # every host with a wg address is a peer
      wg = {
        cidr = "10.100.0.0/24";
        hub = "vps";
        port = 51820;
        mtu = 1420;
      };
    };

    dns = {
      # private zone for internal addresses
      internal = "nya.meowcloud.net";
      # devices get <host>.<devices>.<internal>
      devices = "host";
      # network -> host running its dns resolver
      resolvers = {
        lan = "server2";
        wg = "vps";
      };
      # edge proxies services through wireguard to the outside world
      edge = "vps";
    };

    # hosts declaring meow.server.services, read by each other
    serviceHosts = [
      "server2"
      "vps"
    ];

    kanidm = {
      domain = "idm.meowcloud.net";
      port = 8443;
      replicationPort = 8444;
      # every node runs an oauth2-proxy against itself, its cookie covers what it can protect
      nodes = {
        vps = {
          hostname = infra.kanidm.domain;
          oauth2 = {
            domain = "oauth.meowcloud.net";
            cookieDomain = ".meowcloud.net";
          };
        };
        # public traffic comes in through the vps nginx, the oauth2-proxy stays internal
        server2 = {
          hostname = "home.idm.meowcloud.net";
          oauth2 = {
            domain = "oauth.${infra.dns.internal}";
            cookieDomain = ".${infra.dns.internal}";
          };
          via = "vps";
        };
      };
    };
  };

  inherit (infra)
    hosts
    dns
    networks
    ;

  isHost = name: hasAttr name hosts;
  peers = filter (name: hosts.${name} ? wg) (attrNames hosts);

  # keys managed by wireguard-keys, sops files under ./wireguard
  publicKeys = fromJSON (readFile ./wireguard/public-keys.json);
  wireguard = name: {
    publicKey =
      publicKeys.${name} or (throw "${name} has no wireguard keys, run wireguard-keys keygen");
    privateKey = ./wireguard/private + "/${name}";
    # leaves only, the hub holds every leaf's
    presharedKey = ./wireguard/psk + "/${name}";
  };
in
assert assertMsg (allUnique (map (name: hosts.${name}.wg) peers)) "wg addresses must be unique";
assert assertMsg (hosts.${networks.wg.hub} ? public) "the wg hub needs a public address";
assert assertMsg (all isHost infra.serviceHosts) "service hosts must be known hosts";
assert assertMsg (
  isHost dns.edge && all isHost (attrValues dns.resolvers)
) "dns edge and resolvers must be known hosts";
assert assertMsg (all (net: hosts.${dns.resolvers.${net}} ? ${net}) (
  attrNames dns.resolvers
)) "a resolver must sit on the network it serves";
assert assertMsg (all (name: isHost name && hosts.${name} ? wg) (
  attrNames infra.kanidm.nodes
)) "kanidm nodes must be wg hosts";
recursiveUpdate infra {
  hosts = mapAttrs (
    name: host: host // optionalAttrs (host ? wg) { wireguard = wireguard name; }
  ) hosts;
  networks.wg.endpoint = "${hosts.${networks.wg.hub}.public.v4}:${toString networks.wg.port}";
}
