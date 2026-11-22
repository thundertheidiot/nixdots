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
    filterAttrs
    filter
    fileContents
    findFirst
    mapAttrs
    mapAttrsToList
    getExe
    getExe'
    listToAttrs
    mkIf
    nameValuePair
    optional
    optionalAttrs
    ;
  inherit (builtins) pathExists;

  cfg = config.meow.kanidm;
  inherit (infra.kanidm) domain port;
  replPort = infra.kanidm.replicationPort;

  # certs/<node> = `kanidm-repl-cert` on that node, a missing file leaves the partner out
  nodes = mapAttrs (
    name: node:
    node
    // {
      address = infra.hosts.${name}.wg;
      partner = findFirst (other: other != name) null (attrNames infra.kanidm.nodes);
    }
  ) infra.kanidm.nodes;
  node = nodes.${config.networking.hostName};
  partner = nodes.${node.partner};
  repl = n: "repl://${n.address}:${toString replPort}";
  partnerCert = ./certs/${node.partner};
  certDir = config.security.acme.certs.${node.hostname}.directory;
  proxied = filter (n: (n.via or null) == config.networking.hostName) (attrValues nodes);

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
  options.meow.kanidm.enable = mkEnOpt "kanidm identity management";

  imports = [ ./oauth2-proxy.nix ];

  config = mkIf cfg.enable {
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
          origin = repl node;
          bindaddress = "0.0.0.0:${toString replPort}";
        }
        // optionalAttrs (pathExists partnerCert) {
          ${repl partner} = {
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

    users.users.kanidm.extraGroups = [ "acme" ];

    environment.systemPackages = [
      (pkgs.writeShellScriptBin "kanidm-repl-cert" ''
        ${getExe' pkgs.util-linux "runuser"} -u kanidm -- \
          ${getExe' config.services.kanidm.package "kanidmd"} show-replication-certificate -c /etc/kanidm/server.toml \
          | ${getExe pkgs.gnused} -n 's/^certificate: "\(.*\)"$/\1/p'
      '')
    ];

    security.acme.certs.${node.hostname} = {
      extraDomainNames = [ node.oauth2.domain ] ++ map (n: n.hostname) proxied;
      dnsProvider = "porkbun";
      # the local dnsmasq shadows home.idm
      dnsResolver = "1.1.1.1:53";
      environmentFile = config.sops.secrets.porkbun.path;
      reloadServices = [ "kanidm.service" ];
    };

    systemd.services.kanidm = {
      wants = [ "acme-${node.hostname}.service" ];
      after = [ "acme-${node.hostname}.service" ];
    };

    services.nginx.virtualHosts = {
      ${node.hostname} = {
        forceSSL = true;
        useACMEHost = node.hostname;
        locations."/" = proxy "https://127.0.0.1:${toString port}";
      };
    }
    // listToAttrs (
      map (
        n:
        nameValuePair n.hostname {
          forceSSL = true;
          useACMEHost = node.hostname;
          locations."/" = proxy "https://${n.address}:${toString port}";
        }
      ) proxied
    );

    # lan and wireguard clients skip the vps for the home node
    services.dnsmasq.settings.host-record = mapAttrsToList (
      name: n: "${n.hostname},${n.oauth2.domain},${config.meow.dns.addresses.${name}}"
    ) (filterAttrs (_: n: n ? via) nodes);

    meow.impermanence.directories = [
      {
        path = "/var/lib/kanidm";
        user = "kanidm";
        group = "kanidm";
        permissions = "0700";
      }
    ];
  };
}
