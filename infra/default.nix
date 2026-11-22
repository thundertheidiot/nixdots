let
  inherit (builtins)
    all
    attrNames
    attrValues
    concatMap
    filter
    fromJSON
    hasAttr
    length
    listToAttrs
    mapAttrs
    readFile
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

    services = {
      bazarr = {
        host = "server2";
        internal = "bazarr";
        access = "private";
      };
      bookorbit = {
        host = "server2";
        internal = "books";
        access = "private";
      };
      forgejo = {
        host = "server2";
        internal = "git";
        access = "private";
      };
      homeassistant = {
        host = "server2";
        internal = "homeassistant";
        access = "private";
      };
      homepage = {
        host = "server2";
        internal = "homepage";
        access = "private";
      };
      jellyfin = {
        host = "server2";
        internal = "jellyfin";
        public = [
          "jellyfin.kotiboksi.xyz"
          "jellyfin.meowcloud.net"
        ];
        port = 8096;
      };
      metube = {
        host = "server2";
        internal = "metube";
        access = "private";
      };
      navidrome = {
        host = "server2";
        internal = "navidrome";
        public = [ "navidrome.meowcloud.net" ];
        port = 4533;
      };
      prowlarr = {
        host = "server2";
        internal = "prowlarr";
        access = "private";
      };
      qbittorrent = {
        host = "server2";
        internal = "torrent";
        access = "private";
      };
      radarr = {
        host = "server2";
        internal = "radarr";
        access = "private";
      };
      slskd = {
        host = "server2";
        internal = "soulseek";
        access = "private";
      };
      sonarr = {
        host = "server2";
        internal = "sonarr";
        access = "private";
      };
      soulbeet = {
        host = "server2";
        internal = "soulbeet";
        access = "private";
      };
      syncthing = {
        host = "server2";
        internal = "syncthing";
        access = "private";
      };

      freshrss = {
        host = "vps";
        internal = "rss";
        access = "wg";
      };
      gatus = {
        host = "vps";
        public = [ "meowcloud.net" ];
        port = 8081;
      };
      grafana = {
        host = "vps";
        internal = "dash";
        access = "wg";
      };
      kissasome = {
        host = "vps";
        public = [ "kissasome.meowcloud.net" ];
        port = 23817;
      };
      static = {
        host = "vps";
        public = [ "static.meowcloud.net" ];
      };
      vaultwarden = {
        host = "vps";
        public = [
          "vw.kotiboksi.xyz"
          "vw.meowcloud.net"
        ];
        port = 8222;
      };
      website = {
        host = "vps";
        public = [
          "kotiboksi.xyz"
          "thunder.meowcloud.net"
        ];
        port = 3005;
      };
    };

    kanidm = {
      domain = "idm.meowcloud.net";
      port = 8443;
      replicationPort = 8444;
      # every node runs an oauth2-proxy against itself, its cookie covers what it can protect
      nodes = {
        vps = {
          hostname = "idm.meowcloud.net";
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

    # the collector scrapes exporters on other hosts over wg
    monitoring = {
      collector = "vps";
      storage = {
        host = "server2";
        port = 8428;
      };
      # scraped over wg, these ports open to the hub
      exporters.server2 = {
        node = 9100;
        comin = 4243;
        nginx = 9113;
        forgejo = 9114;
        jellyfin = 8096;
        navidrome = 4533;
        dnsmasq = 9153;
        postgres = 9187;
        radarr = 9708;
        sonarr = 9709;
        prowlarr = 9710;
        bazarr = 9711;
        qbittorrent = 9712;
        slskd = 9713;
      };
    };
  };

  inherit (infra)
    hosts
    services
    dns
    networks
    ;

  unique =
    list:
    length list == length (
      attrNames (
        listToAttrs (
          map (name: {
            inherit name;
            value = null;
          }) list
        )
      )
    );
  isHost = name: hasAttr name hosts;

  # keys managed by wireguard-keys, sops files under ./wireguard
  publicKeys = fromJSON (readFile ./wireguard/public-keys.json);
  wireguard = name: {
    publicKey =
      publicKeys.${name} or (throw "${name} has no wireguard keys, run wireguard-keys keygen");
    privateKey = ./wireguard/private + "/${name}";
    # leaves only, the hub holds every leaf's
    presharedKey = ./wireguard/psk + "/${name}";
  };
  peers = filter (name: hosts.${name} ? wg) (attrNames hosts);
  internal = filter (s: s ? internal) (attrValues services);
in
assert unique (map (name: hosts.${name}.wg) peers);
assert hosts.${networks.wg.hub} ? public;
assert all (s: isHost s.host) (attrValues services);
assert unique (map (s: s.internal) internal);
assert unique (concatMap (s: s.public or [ ]) (attrValues services));
# if public exists, wg must exist
assert all (s: s ? public -> hosts.${s.host} ? wg) (attrValues services);
# if access exists internal domain mapping must exist
assert all (s: s ? access -> s ? internal) (attrValues services);
assert isHost dns.edge && all isHost (attrValues dns.resolvers);
assert all (net: hosts.${dns.resolvers.${net}} ? ${net}) (attrNames dns.resolvers);
assert all (name: isHost name && hosts.${name} ? wg) (attrNames infra.kanidm.nodes);
assert all isHost (attrNames infra.monitoring.exporters);
infra
// {
  hosts = mapAttrs (
    name: host: host // (if host ? wg then { wireguard = wireguard name; } else { })
  ) hosts;
}
