{
  config,
  infra,
  lib,
  mlib,
  pkgs,
  ...
}:
let
  inherit (lib)
    attrNames
    attrValues
    concatMapStringsSep
    concatStrings
    elem
    filter
    filterAttrs
    getExe'
    lessThan
    listToAttrs
    mapAttrs
    mapAttrsToList
    mkIf
    mkMerge
    nameValuePair
    optional
    optionalString
    optionals
    sort
    unique
    ;
  inherit (mlib) mkEnOpt;
  inherit (infra.networks) wg;

  cfg = config.meow.wireguard;
  name = config.networking.hostName;
  peers = mapAttrs (_: host: host.wg) (filterAttrs (_: host: host ? wg) infra.hosts);
  keys = host: infra.hosts.${host}.wireguard;

  isHub = name == wg.hub;
  address = peers.${name};
  hub = peers.${wg.hub};
  leaves = attrNames (removeAttrs peers [ wg.hub ]);

  # ports the hub may open on a leaf
  hubPorts =
    host:
    let
      inherit (config.meow.server) cluster;
      onHost = filter (item: item.host == host);
      hubServices = filter (service: service.port != null && service.expose != [ ]) (
        onHost (attrValues cluster.services)
      );
      kanidm = infra.kanidm.nodes.${host} or null;
    in
    sort lessThan (
      unique (
        optionals (elem host infra.serviceHosts) (
          map (service: service.port) hubServices
          ++ map (exporter: exporter.port) (onHost cluster.exporters)
          ++ optional (cluster.storage != null && cluster.storage.host == host) cluster.storage.port
        )
        ++ optionals (kanidm != null) (
          [ infra.kanidm.replicationPort ] ++ optional (kanidm ? via) infra.kanidm.port
        )
      )
    );
  portSet = ports: "{ ${concatMapStringsSep ", " toString ports} }";
in
{
  options.meow.wireguard = {
    enable = mkEnOpt "WireGuard, as the hub or a leaf depending on the infra inventory";
    dns.enable = mkEnOpt "Private DNS over the home LAN or WireGuard when away";
  };

  config = mkMerge [
    (mkIf cfg.enable {
      sops.secrets =
        let
          secret = sopsFile: {
            inherit sopsFile;
            format = "binary";
            restartUnits = [ "wg-quick-wg0.service" ];
          };
        in
        {
          wg_private = secret (keys name).privateKey;
        }
        // listToAttrs (
          map (leaf: nameValuePair "wg_psk_${leaf}" (secret (keys leaf).presharedKey)) (
            if isHub then leaves else [ name ]
          )
        );

      networking.firewall = {
        backend = "iptables";
        package = pkgs.iptables;
      };

      networking.nftables = {
        enable = true;
        flushRuleset = false;
      };

      systemd.services.wg-quick-wg0 = {
        requires = [ "nftables.service" ];
        after = [ "nftables.service" ];
        partOf = [ "nftables.service" ];
      };
    })

    (mkIf (cfg.enable && isHub) {
      networking.wg-quick.interfaces.wg0 = {
        address = [ "${address}/24" ];
        privateKeyFile = config.sops.secrets.wg_private.path;
        listenPort = wg.port;
        peers = map (leaf: {
          publicKey = (keys leaf).publicKey;
          allowedIPs = [ "${peers.${leaf}}/32" ];
          presharedKeyFile = config.sops.secrets."wg_psk_${leaf}".path;
        }) leaves;
      };

      networking.firewall.allowedUDPPorts = [ wg.port ];

      networking.nftables.tables.wg-hub-boundary = {
        family = "inet";
        content =
          let
            addresses = concatMapStringsSep ", " (leaf: peers.${leaf}) leaves;
          in
          ''
            chain forward {
              type filter hook forward priority -10; policy drop;
              iifname "wg0" oifname "wg0" ip saddr { ${addresses} } ip daddr { ${addresses} } counter accept
            }
            chain output {
              type filter hook output priority -10; policy accept;
              ip saddr { ${addresses} } counter drop
              oifname "wg0" ct state established,related return
            ${concatStrings (
              map (
                leaf:
                optionalString (hubPorts leaf != [ ]) ''
                  oifname "wg0" ip saddr ${address} ip daddr ${peers.${leaf}} tcp dport ${portSet (hubPorts leaf)} return
                ''
              ) leaves
            )}
              oifname "wg0" counter drop
            }
          '';
      };

      # routes between leaves, wg-hub-boundary keeps the hub's own connections to the listed ports
      boot.kernel.sysctl."net.ipv4.ip_forward" = 1;
    })

    (mkIf (cfg.enable && !isHub) (
      let
        tcpPorts = hubPorts name;
      in
      {
        networking.wg-quick.interfaces.wg0 = {
          address = [ "${address}/32" ];
          privateKeyFile = config.sops.secrets.wg_private.path;
          inherit (wg) mtu;
          peers = [
            {
              publicKey = (keys wg.hub).publicKey;
              allowedIPs = mapAttrsToList (_: peer: "${peer}/32") (removeAttrs peers [ name ]);
              presharedKeyFile = config.sops.secrets."wg_psk_${name}".path;
              inherit (wg) endpoint;
              persistentKeepalive = 25;
            }
          ];
        };

        networking.nftables.tables.wg-peer-guard =
          let
            trusted = concatMapStringsSep ", " (leaf: peers.${leaf}) (filter (leaf: leaf != name) leaves);
          in
          {
            family = "inet";
            content = ''
              chain input {
                type filter hook input priority -10; policy accept;
                iifname "wg0" ip saddr { ${trusted} } return
                iifname "wg0" ip saddr ${hub} ct state established,related return
              ${optionalString (
                tcpPorts != [ ]
              ) ''iifname "wg0" ip saddr ${hub} ip daddr ${address} tcp dport ${portSet tcpPorts} return''}
                iifname "wg0" counter drop
              }
              chain forward {
                type filter hook forward priority -10; policy accept;
                iifname "wg0" ip saddr { ${trusted} } ct status dnat ct original ip daddr ${address} return
                iifname "wg0" ct state established,related return
                iifname "wg0" counter drop
              }
            '';
          };

        networking.firewall = {
          trustedInterfaces = [ "wg0" ];
          # hosts that drop wg0 from the trusted interfaces still take the hub's connections
          # one rule per port, multiport caps at 15
          extraCommands = concatStrings (
            map (port: ''
              ${getExe' config.networking.firewall.package "iptables"} -A nixos-fw \
                -i wg0 -s ${hub} -d ${address} -p tcp --dport ${toString port} -j nixos-fw-accept
            '') tcpPorts
          );
        };
      }
    ))

    (mkIf (cfg.dns.enable && !isHub) (
      let
        resolver = peers.${infra.dns.resolvers.wg};
        lanResolver = infra.hosts.${infra.dns.resolvers.lan}.lan;
        ip = getExe' pkgs.iproute2 "ip";
        systemctl = getExe' pkgs.systemd "systemctl";
      in
      {
        services.resolved.enable = false;
        networking.networkmanager = {
          dns = "default";
          # publish upstream DNS without replacing the local dnsmasq resolver
          settings.main.rc-manager = "unmanaged";
        };
        services.dnsmasq = {
          enable = true;
          resolveLocalQueries = true;
          settings = {
            listen-address = [
              "127.0.0.1"
              "::1"
            ];
            bind-interfaces = true;
            resolv-file = "/run/NetworkManager/resolv.conf";
            # respect NetworkManager's DNS priorities, with VPN DNS first by default
            strict-order = true;
            clear-on-reload = true;
            conf-file = [ ];
            servers-file = "/run/wg-dns/servers";
            cache-size = 1000;
          };
        };

        # servers-file is reread on sighup, unlike the main dnsmasq config
        systemd.services.wg-dns-select = {
          description = "Select LAN or VPN private DNS";
          before = [ "dnsmasq.service" ];
          # networkmanager sends enough events at boot to hit the default start limit
          unitConfig.StartLimitIntervalSec = 0;
          serviceConfig = {
            Type = "oneshot";
            RuntimeDirectory = "wg-dns";
            RuntimeDirectoryPreserve = "yes";
          };
          script = ''
            resolver=${resolver}
            if [ -n "$(${ip} -4 route show match ${lanResolver}/32 scope link)" ]; then
              resolver=${lanResolver}
            fi
            printf 'server=/%s/%s\n' ${infra.dns.internal} "$resolver" > /run/wg-dns/servers.new
            if cmp -s /run/wg-dns/servers.new /run/wg-dns/servers; then
              rm /run/wg-dns/servers.new
              exit 0
            fi
            mv /run/wg-dns/servers.new /run/wg-dns/servers
            echo "Private DNS resolver: $resolver"
            if ${systemctl} is-active --quiet dnsmasq.service; then
              ${systemctl} kill --kill-whom=main --signal=HUP dnsmasq.service
            fi
          '';
        };
        # catches dispatcher events missed during boot and resume
        systemd.timers.wg-dns-select = {
          wantedBy = [ "timers.target" ];
          timerConfig = {
            OnBootSec = "15s";
            OnUnitInactiveSec = "30s";
          };
        };
        networking.networkmanager.dispatcherScripts = [
          {
            source = pkgs.writeShellScript "select-private-dns" ''
              case "$2" in
                up|down|vpn-up|vpn-down|dhcp4-change|connectivity-change|reapply)
                  ${systemctl} --no-block start wg-dns-select.service
                  ;;
              esac
            '';
          }
        ];

        systemd.services.wg-dns-route = {
          description = "Fail-closed route for private VPN DNS";
          wantedBy = [ "multi-user.target" ];
          before = [
            "dnsmasq.service"
            "wg-quick-wg0.service"
          ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
          script = ''
            ${ip} route replace blackhole ${resolver}/32 metric 42760
          '';
        };
        systemd.services.dnsmasq = {
          requires = [
            "wg-dns-route.service"
            "wg-dns-select.service"
          ];
          after = [
            "wg-dns-route.service"
            "wg-dns-select.service"
          ];
        };
        systemd.services.wg-quick-wg0 = {
          requires = [ "wg-dns-route.service" ];
          after = [ "wg-dns-route.service" ];
        };
      }
    ))
  ];
}
