{ config, ... }:
let
  nya = config.meow.dns.domain;
in
{
  meow.sops.enableSecrets = [
    "kanidm_oauth2_proxy"
    "oauth2_proxy_cookie"
  ];

  services.kanidm.provision.systems.oauth2.oauth2-proxy = {
    displayName = "Server2";
    originUrl = "https://oauth2.${nya}/oauth2/callback";
    originLanding = "https://homepage.${nya}/";
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
    oidcIssuerUrl = "https://home.idm.meowcloud.net/oauth2/openid/oauth2-proxy";
    redirectURL = "https://oauth2.${nya}/oauth2/callback";
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
      domain = "oauth2.${nya}";
      virtualHosts."sonarr.${nya}" = { };
    };
  };

  services.nginx.virtualHosts."oauth2.${nya}".forceSSL = true;
}
