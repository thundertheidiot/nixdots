{config, ...}: {
  config = {
    services.jellyfin = {
      enable = true;
      openFirewall = true;
      cacheDir = "${config.services.jellyfin.dataDir}/cache";
    };

    meow.impermanence.directories = [
      {
        path = "/var/lib/jellyfin";
        persistPath = "${config.meow.impermanence.persist}/jellyfin";
        user = "jellyfin";
        group = "jellyfin";
      }
    ];

    server.domains = [
      "jellyfin.local"
      "jellyfin.home"
    ];

    services.nginx.virtualHosts."jellyfin.local" = {
      root = "/fake";
      serverAliases = ["jellyfin.home"];

      # recommendedTlsSettings = false;
      locations = {
        "/" = {
          proxyPass = "http://127.0.0.1:8096";
          recommendedProxySettings = false;
          extraConfig = ''
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
            proxy_set_header X-Forwarded-Protocol $scheme;
            proxy_set_header X-Forwarded-Host $http_host;

            # Disable buffering when the nginx proxy gets very resource heavy upon streaming
            proxy_buffering off;
          '';
        };

        "/socket" = {
          proxyPass = "http://127.0.0.1:8096";
          recommendedProxySettings = false;
          extraConfig = ''
            proxy_http_version 1.1;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "upgrade";
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
            proxy_set_header X-Forwarded-Protocol $scheme;
            proxy_set_header X-Forwarded-Host $http_host;
          '';
        };
      };

      extraConfig = ''
                # listen 80;
                # listen [::]:80;

                client_max_body_size 20M;
                add_header X-Frame-Options "SAMEORIGIN";
                add_header X-XSS-Protection "0"; # Do NOT enable. This is obsolete/dangerous ????
                add_header X-Content-Type-Options "nosniff";

                add_header Permissions-Policy "accelerometer=(), ambient-light-sensor=(), battery=(), bluetooth=(), camera=(), clipboard-read=(), display-capture=(), document-domain=(), encrypted-media=(), gamepad=(), geolocation=(), gyroscope=(), hid=(), idle-detection=(), interest-cohort=(), keyboard-map=(), local-fonts=(), magnetometer=(), microphone=(), payment=(), publickey-credentials-get=(), serial=(), sync-xhr=(), usb=(), xr-spatial-tracking=()" always;

                # add_header Content-Security-Policy "default-src https: data: blob: ; img-src 'self' https://* ; style-src 'self' 'unsafe-inline'; script-src 'self' 'unsafe-inline' https://www.gstatic.com https://www.youtube.com blob:; worker-src 'self' blob:; connect-src 'self'; object-src 'none'; frame-ancestors 'self'";

        # add_header Content-Security-Policy "default-src https: data: blob: http://image.tmdb.org; style-src 'self' 'unsafe-inline' https://cdn.jsdelivr.net/npm/jellyskin@latest/dist/main.css; script-src 'self' 'unsafe-inline' https://www.gstatic.com/cv/js/sender/v1/cast_sender.js https://www.youtube.com blob:; worker-src 'self' blob:; connect-src 'self'; object-src 'none'; frame-ancestors 'self'";

        add_header Content-Security-Policy "default-src * 'unsafe-inline' 'unsafe-eval'; script-src * 'unsafe-inline' 'unsafe-eval'; connect-src * 'unsafe-inline'; img-src * data: blob: 'unsafe-inline'; frame-src *; style-src * 'unsafe-inline';";
      '';
    };
  };
}
