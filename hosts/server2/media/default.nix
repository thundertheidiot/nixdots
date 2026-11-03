{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  inherit (lib)
    concatStringsSep
    escapeShellArg
    genAttrs
    getExe
    getExe'
    mapAttrs
    mkForce
    mkIf
    mkMerge
    mkOverride
    nameValuePair
    ;
  stateDir = "/nix/persist/media-stack";
  mediaDir = "/mnt/storage/media";
  arrNames = [
    "radarr"
    "sonarr"
    "prowlarr"
  ];
  appNames = arrNames ++ [
    "qbittorrent"
    "slskd"
    "bazarr"
    "flaresolverr"
  ];
  namespaceAddress = config.vpnNamespaces.airvpn.namespaceAddress;
  certs = import "${inputs.self.outPath}/certs";
  hosts = {
    "torrent.home" = 8080;
    "soulseek.home" = 5030;
    "radarr.home" = 7878;
    "sonarr.home" = 8989;
    "prowlarr.home" = 9696;
    "bazarr.home" = 6767;
  };
  stateRule = user: path: {
    ${path}.d = {
      inherit user;
      group = "media";
      mode = "0700";
    };
  };
in
{
  imports = [
    inputs.nixflix.nixosModules.default
    ./settings.nix
  ];

  # Extend existing units, rather than inventing empty units for absent helpers.
  options.systemd.services = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.submodule (
        { name, ... }: {
          config = mkIf (lib.any (arr: lib.hasPrefix "${arr}-" name) arrNames) {
            vpnConfinement = {
              enable = true;
              vpnNamespace = "airvpn";
            };
            partOf = [ "airvpn.service" ];
            unitConfig.RequiresMountsFor = [
              stateDir
              mediaDir
            ];
            serviceConfig.BindPaths = [
              "${mediaDir}:/media"
              "${mediaDir}/downloads/torrents:/downloads"
            ];
          };
        }
      )
    );
  };

  config = mkMerge [
    {
      # This gate owns UI access; never apply it to public Jellyfin/Navidrome.
      services.nginx.virtualHosts =
        genAttrs
          [
            "torrent.home"
            "soulseek.home"
            "radarr.home"
            "sonarr.home"
            "prowlarr.home"
            "bazarr.home"
            "metube.home"
            "soulbeet.home"
          ]
          (_: {
            extraConfig = ''
              allow 192.168.101.0/24;
              allow 10.100.0.0/24;
              allow 127.0.0.1;
              allow ::1;
              deny all;
            '';
          });
    }
    {
      nixflix = {
        enable = true;
        inherit stateDir mediaDir;
        downloadsDir = "${mediaDir}/downloads";
        # Keep the existing TLS endpoints, not nixflix's auto-generated vhosts.
        nginx.enable = false;
        caddy.enable = false;
        postgres.enable = false;
        recyclarr.enable = false;
        vpn = {
          enable = true;
          namespace = "airvpn";
          wgConfFile = config.sops.secrets.media-stack-wireguard.path;
          accessibleFrom = [ "192.168.101.0/24" ];
          openVPNPorts = [
            {
              port = 3865;
              protocol = "both";
            }
          ];
        };
        torrentClients.qbittorrent = {
          enable = true;
          vpn.enable = true;
          profileDir = "${stateDir}/qbittorrent";
          downloadsDir = "${mediaDir}/downloads/torrents";
          webuiPort = 8080;
          torrentingPort = 3865;
          # Preserve migrated category paths and torrent-management behavior.
          categories = mkForce { };
        };
        slskd = {
          enable = true;
          vpn.enable = true;
          downloadsDir = "${mediaDir}/downloads/soulseek";
          settings.directories = {
            downloads = "/app/downloads";
            incomplete = "/app/incomplete";
          };
        };
        flaresolverr.enable = true;
      };

      # No direct/LAN egress exception: every new outgoing connection uses WG.
      # nginx connects directly to the namespace; no broad host-port DNAT needed.
      vpnNamespaces.airvpn = {
        allowedEgress = mkForce [ ];
        portMappings = mkForce [ ];
      };
      systemd.services.airvpn.postStart =
        let
          inherit (lib) concatMapStringsSep optionalString;
          ports = lib.unique (builtins.attrValues hosts);

          ip = getExe' pkgs.iproute2 "ip";
          ipTables = getExe pkgs.iptables;
          ip6Tables = getExe' pkgs.iptables "ip6tables";

          firewall = tool: bridgeAddress: ''
            ${concatMapStringsSep "\n" (port: ''
              ${ip} netns exec airvpn ${tool} -A INPUT \
                -i veth-airvpn -s ${bridgeAddress} -p tcp --dport ${toString port} -j ACCEPT
            '') ports}
            ${ip} netns exec airvpn ${tool} -A OUTPUT -o lo -j ACCEPT
            ${ip} netns exec airvpn ${tool} -A OUTPUT -o airvpn0 -j ACCEPT
            ${ip} netns exec airvpn ${tool} -P OUTPUT DROP
          '';
        in
        firewall ipTables config.vpnNamespaces.airvpn.bridgeAddress
        + optionalString config.networking.enableIPv6 (
          firewall ip6Tables config.vpnNamespaces.airvpn.bridgeAddressIPv6
          # OUTPUT DROP also blocks neighbour discovery needed for host UI replies.
          # Permit only link-local control messages, not new application traffic.
          +
            concatMapStringsSep "\n"
              (icmpType: ''
                ${ip} netns exec airvpn ${ip6Tables} \
                  -I OUTPUT 1 -o veth-airvpn -p ipv6-icmp --icmpv6-type ${icmpType} \
                  -m hl --hl-eq 255 -j ACCEPT
              '')
              [
                "135"
                "136"
              ]
        );

      # Whole-record overrides are needed against the upstream account definitions.
      # Keep copied-state ownership and the service homes stable across activation.
      users.groups.media = mkForce { gid = config.nixflix.globals.gids.media; };
      users.users.qbittorrent = mkForce {
        uid = config.nixflix.globals.uids.qbittorrent;
        group = "media";
        home = "${stateDir}/qbittorrent";
        isSystemUser = true;
      };
      users.users.slskd = mkForce {
        uid = config.nixflix.globals.uids.slskd;
        group = "media";
        home = "${stateDir}/slskd/app";
        isSystemUser = true;
      };
      users.users.bazarr.uid = mkForce config.nixflix.globals.uids.bazarr;

      services.qbittorrent.serverConfig = mkForce {
        WebUI = {
          LocalHostAuth = false;
          AuthSubnetWhitelistEnabled = true;
          AuthSubnetWhitelist = concatStringsSep ", " [
            "${config.vpnNamespaces.airvpn.bridgeAddress}/32"
            "192.168.101.0/24"
            "10.100.0.0/24"
          ];
        };
      };
      # nixpkgs normally replaces the entire file. Merge our keys while stopped,
      # preserving the migrated profile, password hash, paths and torrent settings.
      systemd.services.qbittorrent.serviceConfig.ExecStartPre =
        let
          inherit (lib.generators) toINI;
          settings = pkgs.writeText "qbittorrent-webui.conf" (
            toINI { } config.services.qbittorrent.serverConfig
          );
        in
        mkForce "${pkgs.writeShellScript "qbittorrent-merge-config" ''
          set -eu
          profile=${escapeShellArg "${config.services.qbittorrent.profileDir}/qBittorrent/config/qBittorrent.conf"}
          test -f "$profile"
          ${getExe pkgs.crudini} --merge "$profile" < ${settings}
        ''}";
      services.bazarr = {
        enable = true;
        dataDir = "${stateDir}/bazarr";
        group = "media";
        settings.general.port = 6767;
        settings.auth.type = null;
      };

      systemd.services.nixflix-setup-dirs = {
        unitConfig.RequiresMountsFor = [
          stateDir
          mediaDir
        ];
      };
      systemd.services.slskd.serviceConfig = {
        # Upstream fixes app-dir at /var/lib/slskd. Bind only this service's view.
        StateDirectory = mkForce "";
        LoadCredential = [ "slskd-config:/run/media-stack/slskd.yml" ];
        ExecStart = mkForce "${config.services.slskd.package}/bin/slskd --app-dir /var/lib/slskd --config /run/credentials/slskd.service/slskd-config";
        BindPaths = [
          "${stateDir}/slskd/app:/var/lib/slskd"
          "${stateDir}/slskd/app:/app"
          "${mediaDir}/downloads/soulseek:/app/downloads"
          "${mediaDir}/downloads/soulseek/incomplete:/app/incomplete"
        ];
        ReadWritePaths = [ "${stateDir}/slskd/app" ];
      };

      # Do not let global tmpfiles create legacy aliases or chmod library roots.
      systemd.tmpfiles.settings = {
        "10-nixflix" = mkForce {
          ${stateDir}.d = {
            mode = "0755";
            user = "root";
            group = "root";
          };
        };
        "10-qbittorrent" = mkForce (stateRule "qbittorrent" "${stateDir}/qbittorrent");
        "10-slskd" = mkForce (
          (stateRule "slskd" "${stateDir}/slskd")
          // (stateRule "slskd" "${stateDir}/slskd/app")
          // {
            "/run/slskd".d = {
              mode = "0700";
              user = "slskd";
              group = "media";
            };
          }
        );
      };

      server.domains = builtins.attrNames hosts;
      services.nginx.virtualHosts = mapAttrs (_: port: {
        addSSL = true;
        sslCertificate = certs."local.crt";
        sslCertificateKey = config.sops.secrets.localKey.path;
        locations."/" = {
          proxyPass = "http://${namespaceAddress}:${toString port}";
          recommendedProxySettings = true;
          proxyWebsockets = true;
          extraConfig = lib.optionalString (port == 5030) ''
            proxy_request_buffering off;
            client_max_body_size 0;
          '';
        };
      }) hosts;
    }
    {
      nixflix = genAttrs arrNames (
        name:
        {
          enable = true;
          vpn.enable = true;
          group = "media";
        }
        // lib.optionalAttrs (name != "prowlarr") {
          # Default rootfolder reconciliation deletes undeclared roots. Only run
          # when settings.nix explicitly provides the reviewed exported roots.
          config.rootFolders = mkOverride 900 [ ];
        }
      );
      systemd.tmpfiles.settings = builtins.listToAttrs (
        map (name: nameValuePair "10-${name}" (mkForce (stateRule name "${stateDir}/${name}"))) arrNames
      );
    }
    {
      systemd.services = genAttrs appNames (name: {
        # BindsTo + After supplied by VPN-Confinement stops services on teardown;
        # PartOf also propagates explicit namespace restarts without host fallback.
        partOf = [ "airvpn.service" ];
        vpnConfinement = {
          enable = true;
          vpnNamespace = "airvpn";
        };
        unitConfig.RequiresMountsFor = [
          stateDir
          mediaDir
        ];
        serviceConfig = {
          BindPaths = [
            "${mediaDir}:/media"
            "${mediaDir}/downloads/torrents:/downloads"
          ];
        };
      });
    }
  ];
}
