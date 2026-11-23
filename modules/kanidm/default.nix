{
  config,
  lib,
  mlib,
  pkgs,
  infra,
  ...
}:
let
  inherit (mlib) mkEnOpt;
  inherit (lib)
    attrNames
    attrValues
    fileContents
    filter
    filterAttrs
    findFirst
    getExe
    getExe'
    listToAttrs
    mapAttrs
    mapAttrsToList
    mkIf
    mkMerge
    nameValuePair
    optional
    optionalAttrs
    ;
  inherit (builtins) pathExists;

  cfg = config.meow.kanidm;
  inherit (infra.kanidm) domain port replicationPort;

  nodes = mapAttrs (
    name: node:
    node
    // {
      address = infra.hosts.${name}.wg;
      partner = findFirst (other: other != name) null (attrNames infra.kanidm.nodes);
    }
  ) infra.kanidm.nodes;
  node = nodes.${config.networking.hostName};
  # nodes reaching the internet through this one
  proxied = filter (peer: (peer.via or null) == config.networking.hostName) (attrValues nodes);
in
{
  options.meow.kanidm.enable = mkEnOpt "kanidm identity management";

  imports = [ ./oauth2-proxy.nix ];

  config = mkIf cfg.enable (mkMerge [
    (
      let
        # certs/<node> = `kanidm-repl-cert` on that node, a missing file leaves the partner out
        partnerCert = ./certs/${node.partner};
        replicationUri = peer: "repl://${peer.address}:${toString replicationPort}";
        certDir = config.security.acme.certs.${node.hostname}.directory;
      in
      {
        services.kanidm = {
          package = pkgs.kanidmWithSecretProvisioning_1_12;

          server.enable = true;
          server.settings = {
            inherit domain;
            origin = "https://${node.hostname}";
            # proxied nodes also take the via node's requests over wireguard
            bindaddress = "${if node ? via then "0.0.0.0" else "127.0.0.1"}:${toString port}";
            tls_chain = "${certDir}/fullchain.pem";
            tls_key = "${certDir}/key.pem";
            http_client_address_info.x-forward-for = [
              "127.0.0.1"
            ]
            ++ optional (node ? via) nodes.${node.via}.address;

            replication = {
              origin = replicationUri node;
              bindaddress = "0.0.0.0:${toString replicationPort}";
            }
            // optionalAttrs (pathExists partnerCert) {
              ${replicationUri nodes.${node.partner}} = {
                type = "mutual-pull";
                partner_cert = fileContents partnerCert;
                # the provisioning node is the primary, the other one refreshes from it
                automatic_refresh = !config.services.kanidm.provision.enable;
              };
            };
          };

          client.enable = true;
          client.settings.uri = "https://${node.hostname}";
        };

        networking.firewall.interfaces.wg0.allowedTCPPorts = [ replicationPort ];

        environment.systemPackages = [
          (pkgs.writeShellScriptBin "kanidm-repl-cert" ''
            ${getExe' pkgs.util-linux "runuser"} -u kanidm -- \
              ${getExe' config.services.kanidm.package "kanidmd"} show-replication-certificate -c /etc/kanidm/server.toml \
              | ${getExe pkgs.gnused} -n 's/^certificate: "\(.*\)"$/\1/p'
          '')
        ];

        meow.impermanence.directories = [
          {
            path = "/var/lib/kanidm";
            user = "kanidm";
            group = "kanidm";
            permissions = "0700";
          }
        ];
      }
    )

    {
      meow.server.dnsCertificates = [ node.hostname ];
      security.acme.certs.${node.hostname} = {
        extraDomainNames = [ node.oauth2.domain ] ++ map (peer: peer.hostname) proxied;
        reloadServices = [ "kanidm.service" ];
      };

      users.users.kanidm.extraGroups = [ "acme" ];

      systemd.services.kanidm = {
        wants = [ "acme-${node.hostname}.service" ];
        after = [ "acme-${node.hostname}.service" ];
      };
    }

    (
      let
        # overwrite xff so kanidm only ever sees the real client
        proxy = target: {
          proxyPass = target;
          recommendedProxySettings = false;
          extraConfig = ''
            proxy_set_header Host $host;
            proxy_set_header X-Forwarded-For $remote_addr;
            proxy_set_header X-Forwarded-Proto $scheme;
          '';
        };
      in
      {
        services.nginx.virtualHosts = {
          ${node.hostname} = {
            forceSSL = true;
            useACMEHost = node.hostname;
            locations."/" = proxy "https://127.0.0.1:${toString port}";
          };
        }
        // listToAttrs (
          map (
            peer:
            nameValuePair peer.hostname {
              forceSSL = true;
              useACMEHost = node.hostname;
              locations."/" = proxy "https://${peer.address}:${toString port}";
            }
          ) proxied
        );

        # lan and wireguard clients skip the vps for the home node
        services.dnsmasq.settings.host-record = mapAttrsToList (
          name: peer: "${peer.hostname},${peer.oauth2.domain},${config.meow.dns.addresses.${name}}"
        ) (filterAttrs (_: peer: peer ? via) nodes);
      }
    )
  ]);
}
