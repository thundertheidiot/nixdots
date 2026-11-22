{
  config,
  infra,
  lib,
  ...
}:
let
  inherit (lib) genAttrs;
  inherit (infra.services.navidrome) public;
in
{
  config = {
    # kanidm login picks the navidrome user, subsonic clients keep their own auth
    meow.kanidm.protect = genAttrs public (_: [ "users" ]);
    services.nginx.virtualHosts = genAttrs public (domain: {
      locations."/" = {
        proxyWebsockets = true;
        extraConfig = ''
          auth_request_set $username $upstream_http_x_auth_request_preferred_username;
          proxy_set_header Remote-User $username;
        '';
      };
      locations."/rest/" = {
        proxyPass = config.meow.server.reverseProxy.${domain};
        recommendedProxySettings = true;
        extraConfig = ''
          auth_request off;
          proxy_set_header Remote-User "";
        '';
      };
    });
  };
}
