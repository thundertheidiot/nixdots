{
  config,
  lib,
  mlib,
  ...
}:
let
  inherit (lib)
    concatStrings
    hasPrefix
    head
    mapAttrs
    optionalString
    removePrefix
    ;
  inherit (mlib) hexToHsl;

  inherit (config.meow.dns) names;
  inherit (config.meow.server.cluster) services;
  port = 8090;
in
{
  config = {
    meow.server.services.glance.access = "wg";

    services.nginx.virtualHosts.${names.glance}.locations."/".proxyPass =
      "http://127.0.0.1:${toString port}";

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
            hsl =
              color:
              let
                components = hexToHsl color;
              in
              "${toString components.h} ${toString components.s} ${toString components.l}";
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
                      serviceName: title: icon:
                      let
                        service = services.${serviceName};
                        anchor =
                          url: class: text:
                          ''<a href="${url}" class="${class}" target="_blank" rel="noreferrer">${text}</a>'';
                        primary =
                          if service.internal != null then
                            "https://${names.${serviceName}}"
                          else
                            "https://${head service.expose}";
                      in
                      ''
                        <li><div class="flex items-center gap-10">
                          <div class="bookmarks-icon-container"><img class="bookmarks-icon${optionalString (hasPrefix "mdi:" icon) " flat-icon"}" src="${iconUrl icon}" alt="" loading="lazy"></div>
                          ${anchor primary "bookmarks-link color-highlight size-h4" title}
                          ${optionalString (service.internal != null && service.expose != [ ]) (
                            anchor "https://${head service.expose}" "bookmarks-link-no-arrow color-subdue size-h6" "public"
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
                    title-url = "https://${head services.gatus.expose}";
                    cache = "1m";
                    url = "http://127.0.0.1:${toString services.gatus.port}/api/v1/endpoints/statuses?pageSize=1";
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
