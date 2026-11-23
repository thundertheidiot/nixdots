{
  config,
  lib,
  mlib,
  infra,
  ...
}:
let
  inherit (lib)
    mapAttrs
    mapAttrs'
    mkIf
    mkMerge
    nameValuePair
    ;
  inherit (mlib) mkOpt;
  inherit (lib.types) attrsOf listOf str;

  cfg = config.meow.kanidm;
  name = config.networking.hostName;
  inherit (infra.kanidm) nodes;
  node = nodes.${name};
  client = host: "oauth2-proxy-${host}";
in
{
  options.meow.kanidm.protect = mkOpt (attrsOf (listOf str)) { } {
    example = {
      "sonarr.nya.meowcloud.net" = [ "admins" ];
    };
    description = "nginx vhosts behind the local oauth2-proxy, with the kanidm groups allowed in";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      meow.sops.enableSecrets = [
        "kanidm_oauth2_proxy"
        "oauth2_proxy_cookie"
      ];

      services.oauth2-proxy = {
        enable = true;
        provider = "oidc";
        clientID = client name;
        clientSecretFile = config.sops.secrets.kanidm_oauth2_proxy.path;
        # the local node, so login keeps working without the other host
        oidcIssuerUrl = "https://${node.hostname}/oauth2/openid/${client name}";
        redirectURL = "https://${node.oauth2.domain}/oauth2/callback";
        scope = "openid email profile groups";
        email.domains = [ "*" ];
        approvalPrompt = "auto";
        reverseProxy = true;
        trustedProxyIP = [ "127.0.0.1" ];
        setXauthrequest = true;

        cookie = {
          name = "_oauth2_proxy_${name}";
          domain = node.oauth2.cookieDomain;
          secretFile = config.sops.secrets.oauth2_proxy_cookie.path;
        };

        extraConfig = {
          code-challenge-method = "S256";
          whitelist-domain = node.oauth2.cookieDomain;
          skip-provider-button = true;
          # kanidm leaves out email for persons without one
          oidc-email-claim = "preferred_username";
        };

        nginx = {
          domain = node.oauth2.domain;
          # the groups claim carries spns
          virtualHosts = mapAttrs (_: groups: {
            allowed_groups = map (group: "${group}@${infra.kanidm.domain}") groups;
          }) cfg.protect;
        };
      };

      services.nginx.virtualHosts.${node.oauth2.domain} = {
        forceSSL = true;
        useACMEHost = node.hostname;
      };
    }
    {
      services.kanidm.provision.systems.oauth2 = mapAttrs' (
        host: n:
        nameValuePair (client host) {
          displayName = "OAuth2 Proxy (${host})";
          originUrl = "https://${n.oauth2.domain}/oauth2/callback";
          originLanding = "https://${config.meow.dns.names.homepage}/";
          basicSecretFile = config.sops.secrets.kanidm_oauth2_proxy.path;
          preferShortUsername = true;
          scopeMaps.users = [
            "openid"
            "email"
            "profile"
            "groups"
          ];
        }
      ) nodes;
    }
  ]);
}
