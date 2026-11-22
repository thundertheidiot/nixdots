{ config, infra, ... }:
let
  nya = config.meow.dns.domain;
  inherit (config.meow.dns) names;
  idm = infra.kanidm.nodes.server2.hostname;
in
{
  meow.sops.enableSecrets = [
    "kanidm_oauth2_proxy"
    "oauth2_proxy_cookie"
  ];

  services.kanidm.provision.systems.oauth2.oauth2-proxy = {
    displayName = "Server2";
    originUrl = "https://${names.oauth2-proxy}/oauth2/callback";
    originLanding = "https://${names.homepage}/";
    basicSecretFile = config.sops.secrets.kanidm_oauth2_proxy.path;
    preferShortUsername = true;
    scopeMaps.admins = [
      "openid"
      "email"
      "profile"
      "groups"
    ];
  };

  services.oauth2-proxy = {
    enable = true;
    provider = "oidc";
    clientID = "oauth2-proxy";
    clientSecretFile = config.sops.secrets.kanidm_oauth2_proxy.path;
    oidcIssuerUrl = "https://${idm}/oauth2/openid/oauth2-proxy";
    redirectURL = "https://${names.oauth2-proxy}/oauth2/callback";
    scope = "openid email profile groups";
    email.domains = [ "*" ];
    approvalPrompt = "auto";
    reverseProxy = true;
    trustedProxyIP = [ "127.0.0.1" ];
    setXauthrequest = true;

    cookie = {
      domain = ".${nya}";
      secretFile = config.sops.secrets.oauth2_proxy_cookie.path;
    };

    extraConfig = {
      code-challenge-method = "S256";
      whitelist-domain = ".${nya}";
      skip-provider-button = true;
    };

    nginx = {
      domain = names.oauth2-proxy;
      virtualHosts.${names.sonarr} = { };
    };
  };

  services.nginx.virtualHosts.${names.oauth2-proxy}.forceSSL = true;
}
