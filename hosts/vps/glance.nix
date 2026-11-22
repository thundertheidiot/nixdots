{
  config,
  infra,
  lib,
  mlib,
  ...
}:
let
  inherit (lib)
    concatStrings
    hasPrefix
    head
    last
    mapAttrs
    optionalString
    removePrefix
    ;
  inherit (mlib) hexToHsl;

  inherit (config.meow.dns) names;
  glance = names.glance;
  port = 8090;
in
{
  config = {
    services.nginx.virtualHosts.${glance} = {
      onlySSL = true;
      listen = [
        {
          addr = infra.hosts.vps.wg;
          port = 443;
          ssl = true;
        }
      ];
      locations."/".proxyPass = "http://127.0.0.1:${toString port}";
    };

    sops.secrets.glance_env = {
      sopsFile = ./secrets/glance.env;
      format = "dotenv";
    };

    services.glance = {
      enable = true;
      environmentFile = config.sops.secrets.glance_env.path;
      settings = {
        server = {
          host = "127.0.0.1";
          inherit port;
        };

        theme =
          let
            inherit (config.meow.workstation.theme.palette) dark roles;
            hsl = color: with hexToHsl color; "${toString h} ${toString s} ${toString l}";
          in
          {
            light = !dark;
            disable-picker = true;
          }
          // mapAttrs (_: hsl) {
            background-color = roles.background;
            primary-color = roles.accent;
            positive-color = roles.success;
            negative-color = roles.error;
          };

        pages = [
          {
            name = "Home";
            hide-desktop-navigation = true;
            columns = [
              {
                size = "small";
                widgets = [
                  {
                    type = "calendar";
                    first-day-of-week = "monday";
                  }
                  {
                    type = "server-stats";
                    servers = [
                      {
                        type = "local";
                        name = "vps";
                      }
                    ];
                  }
                ];
              }
              {
                size = "full";
                widgets =
                  let
                    # same urls glance builds for its icon prefixes
                    iconUrl =
                      icon:
                      if hasPrefix "mdi:" icon then
                        "https://cdn.jsdelivr.net/npm/@mdi/svg@latest/svg/${removePrefix "mdi:" icon}.svg"
                      else
                        "https://cdn.jsdelivr.net/gh/homarr-labs/dashboard-icons/svg/${removePrefix "di:" icon}.svg";

                    # internal address first, public one as a secondary link when both exist
                    link =
                      service: title: icon:
                      let
                        s = infra.services.${service};
                        a =
                          url: class: text:
                          ''<a href="${url}" class="${class}" target="_blank" rel="noreferrer">${text}</a>'';
                        # newest public domain is listed last
                        primary = if s ? internal then "https://${names.${service}}" else "https://${last s.public}";
                      in
                      ''
                        <li><div class="flex items-center gap-10">
                          <div class="bookmarks-icon-container"><img class="bookmarks-icon${optionalString (hasPrefix "mdi:" icon) " flat-icon"}" src="${iconUrl icon}" alt="" loading="lazy"></div>
                          ${a primary "bookmarks-link color-highlight size-h4" title}
                          ${optionalString (s ? internal && s ? public) (
                            a "https://${last s.public}" "bookmarks-link-no-arrow color-subdue size-h6" "public"
                          )}
                        </div></li>
                      '';

                    group = title: links: ''
                      <div class="bookmarks-group">
                        <div class="bookmarks-group-title size-h3 margin-bottom-3">${title}</div>
                        <ul class="list list-gap-2">${concatStrings links}</ul>
                      </div>
                    '';
                  in
                  [
                    {
                      type = "search";
                      search-engine = "duckduckgo";
                      autofocus = true;
                    }
                    {
                      # bookmarks widget markup, it can't hold two links per entry
                      type = "custom-api";
                      title = "Services";
                      cache = "1d";
                      template = ''
                        <div class="dynamic-columns list-gap-24 list-with-separator">
                        ${group "Media" [
                          (link "jellyfin" "Jellyfin" "di:jellyfin")
                          (link "navidrome" "Navidrome" "di:navidrome")
                          (link "freshrss" "FreshRSS" "di:freshrss")
                          (link "bookorbit" "BookOrbit" "mdi:book-open-page-variant")
                        ]}
                        ${group "Downloads" [
                          (link "radarr" "Radarr" "di:radarr")
                          (link "sonarr" "Sonarr" "di:sonarr")
                          (link "bazarr" "Bazarr" "di:bazarr")
                          (link "prowlarr" "Prowlarr" "di:prowlarr")
                          (link "qbittorrent" "qBittorrent" "di:qbittorrent")
                          (link "slskd" "Soulseek" "di:slskd")
                          (link "soulbeet" "Soulbeet" "mdi:music-box-multiple")
                          (link "metube" "MeTube" "di:metube")
                        ]}
                        ${group "Services" [
                          (link "forgejo" "Forgejo" "di:forgejo")
                          (link "homeassistant" "Home Assistant" "di:home-assistant")
                          (link "syncthing" "Syncthing" "di:syncthing")
                          (link "grafana" "Grafana" "di:grafana")
                          (link "vaultwarden" "Vaultwarden" "di:vaultwarden")
                          (link "gatus" "Gatus" "di:gatus")
                        ]}
                        </div>
                      '';
                    }
                  ];
              }
              {
                size = "small";
                widgets = [
                  {
                    type = "weather";
                    location = "\${GLANCE_LOCATION}";
                    units = "metric";
                    hour-format = "24h";
                  }
                  {
                    # latest gatus result per endpoint
                    type = "custom-api";
                    title = "Status";
                    title-url = "https://${head infra.services.gatus.public}";
                    cache = "1m";
                    url = "http://127.0.0.1:${toString infra.services.gatus.port}/api/v1/endpoints/statuses?pageSize=1";
                    template = ''
                      <ul class="list list-gap-10">
                      {{ range .JSON.Array "" }}
                        <li class="flex justify-between items-center">
                          <span class="color-highlight">{{ .String "name" }}</span>
                          {{ if .Bool "results.0.success" }}
                            <span class="color-positive">{{ div (.Int "results.0.duration" | toFloat) 1000000 | printf "%.0fms" }}</span>
                          {{ else }}
                            <span class="color-negative">down</span>
                          {{ end }}
                        </li>
                      {{ end }}
                      </ul>
                    '';
                  }
                ];
              }
            ];
          }
        ];
      };
    };
  };
}
