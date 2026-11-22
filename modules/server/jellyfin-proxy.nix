{
  config,
  infra,
  lib,
  ...
}:
let
  inherit (lib)
    genAttrs
    mkIf
    mkMerge
    recursiveUpdate
    ;

  name = config.networking.hostName;
  jellyfin = infra.services.jellyfin;
  isEdge = name == infra.dns.edge;

  headers = ''
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header X-Forwarded-Proto $scheme;
    proxy_set_header X-Forwarded-Protocol $scheme;
    proxy_set_header X-Forwarded-Host $http_host;
  '';

  vhost = target: {
    locations = {
      "/" = {
        proxyPass = target;
        recommendedProxySettings = false;
        extraConfig = headers + ''
          # Disable buffering when the nginx proxy gets very resource heavy upon streaming
          proxy_buffering off;
        '';
      };

      "/socket" = {
        proxyPass = target;
        recommendedProxySettings = false;
        extraConfig = ''
          proxy_http_version 1.1;
          proxy_set_header Upgrade $http_upgrade;
          proxy_set_header Connection "upgrade";
        ''
        + headers;
      };
    };

    extraConfig = ''
      client_max_body_size 20M;
      add_header X-Frame-Options "SAMEORIGIN";
      add_header X-XSS-Protection "0"; # Do NOT enable. This is obsolete/dangerous
      add_header X-Content-Type-Options "nosniff";

      add_header Permissions-Policy "accelerometer=(), ambient-light-sensor=(), battery=(), bluetooth=(), camera=(), clipboard-read=(), display-capture=(), document-domain=(), encrypted-media=(), gamepad=(), geolocation=(), gyroscope=(), hid=(), idle-detection=(), interest-cohort=(), keyboard-map=(), local-fonts=(), magnetometer=(), microphone=(), payment=(), publickey-credentials-get=(), serial=(), sync-xhr=(), usb=(), xr-spatial-tracking=()" always;

      add_header Content-Security-Policy "default-src * 'unsafe-inline' 'unsafe-eval'; script-src * 'unsafe-inline' 'unsafe-eval'; connect-src * 'unsafe-inline'; img-src * data: blob: 'unsafe-inline'; frame-src *; style-src * 'unsafe-inline';";
    '';
  };
in
{
  config = mkMerge [
    (mkIf (name == jellyfin.host && config.meow.dns.enable) {
      services.nginx.virtualHosts.${config.meow.dns.names.jellyfin} =
        vhost "http://127.0.0.1:${toString jellyfin.port}"
        // {
          forceSSL = true;
        };
    })
    (mkIf (isEdge && name != jellyfin.host) {
      services.nginx.virtualHosts = genAttrs jellyfin.public (
        _:
        recursiveUpdate (vhost "http://${infra.hosts.${jellyfin.host}.wg}:${toString jellyfin.port}") {
          locations."= /metrics".return = "404";
        }
      );
    })
  ];
}
