{ lib, pkgs, ... }:
let
  inherit (builtins) map toJSON;
  inherit (lib) recursiveUpdate;
  datasource = {
    type = "prometheus";
    uid = "\${datasource}";
  };
  query = refId: expr: legendFormat: { inherit refId expr legendFormat; };
  at =
    x: y: w: h: panel:
    panel
    // {
      gridPos = {
        inherit
          x
          y
          w
          h
          ;
      };
    };
  panel = id: title: unit: targets: {
    inherit
      id
      title
      datasource
      targets
      ;
    type = "timeseries";
    fieldConfig.defaults = {
      inherit unit;
      noValue = "No data";
      min = 0;
      color.mode = "palette-classic";
      custom = {
        lineWidth = 2;
        fillOpacity = 8;
        showPoints = "never";
      };
    };
    fieldConfig.overrides = [
      {
        matcher = {
          id = "byRegexp";
          options = ".*[Dd]ownload.*";
        };
        properties = [
          {
            id = "color";
            value = {
              mode = "fixed";
              fixedColor = "blue";
            };
          }
        ];
      }
      {
        matcher = {
          id = "byRegexp";
          options = ".*[Uu]pload.*";
        };
        properties = [
          {
            id = "color";
            value = {
              mode = "fixed";
              fixedColor = "purple";
            };
          }
        ];
      }
    ];
    options = {
      tooltip.mode = "multi";
      legend = {
        displayMode = "list";
        placement = "bottom";
      };
    };
  };
  stat = id: title: unit: targets: {
    inherit id title datasource;
    targets = map (target: target // { instant = true; }) targets;
    type = "stat";
    fieldConfig.defaults = {
      inherit unit;
      noValue = "No data";
      color = {
        mode = "fixed";
        fixedColor = "text";
      };
      decimals = if unit == "short" then 0 else 1;
    };
    options = {
      colorMode = "value";
      graphMode = "none";
      textMode = "auto";
      reduceOptions = {
        calcs = [ "lastNotNull" ];
        values = false;
      };
    };
  };
  problems =
    id: title: targets:
    recursiveUpdate (stat id title "short" targets) {
      fieldConfig.defaults = {
        color.mode = "thresholds";
        thresholds = {
          mode = "absolute";
          steps = [
            {
              color = "text";
              value = null;
            }
            {
              color = "red";
              value = 1;
            }
          ];
        };
      };
    };
  statusMappings = [
    {
      type = "value";
      options = {
        "0" = {
          text = "DOWN";
          color = "red";
        };
        "1" = {
          text = "OK";
          color = "green";
        };
      };
    }
  ];
  row = id: title: y: collapsed: panels: {
    inherit
      id
      title
      collapsed
      panels
      ;
    type = "row";
    gridPos = {
      x = 0;
      inherit y;
      w = 24;
      h = 1;
    };
  };
  gatusStatus = {
    id = 6;
    title = "Endpoint availability";
    inherit datasource;
    type = "state-timeline";
    targets = [ (query "A" "gatus_results_endpoint_success" "{{group}} {{name}}") ];
    fieldConfig.defaults = {
      noValue = "No data";
      mappings = statusMappings;
    };
    options = {
      mergeValues = true;
      showValue = "never";
      rowHeight = 0.8;
    };
  };
in
{
  config.services.grafana.provision.dashboards.settings.providers = [
    {
      name = "services";
      options.path = pkgs.writeTextDir "services.json" (toJSON {
        uid = "vps-services";
        title = "Services";
        schemaVersion = 39;
        version = 3;
        refresh = "30s";
        time = {
          from = "now-6h";
          to = "now";
        };
        templating.list = [
          {
            name = "datasource";
            label = "Metrics source";
            type = "datasource";
            query = "prometheus";
            current = {
              text = "VPS Prometheus";
              value = "vps-prometheus";
            };
          }
        ];
        annotations.list = [
          {
            builtIn = 1;
            datasource.type = "grafana";
            datasource.uid = "-- Grafana --";
            enable = true;
            hide = true;
            iconColor = "red";
            name = "Annotations & Alerts";
            type = "dashboard";
          }
          {
            name = "Target state changed";
            datasource = {
              type = "prometheus";
              uid = "\${datasource}";
            };
            enable = true;
            expr = "changes(up[1m]) > 0";
            iconColor = "orange";
            titleFormat = "{{job}} {{instance}} changed state";
          }
        ];
        panels = [
          (row 200 "Overview" 0 false [ ])
          (at 0 1 6 4 (
            problems 104 "Failing endpoints · last 5m" [
              (query "A" "sum(1 - min_over_time(gatus_results_endpoint_success[5m]))" "Endpoints")
            ]
          ))
          (at 6 1 6 4 (
            problems 105 "Media health issues" [
              (query "A" "sonarr_system_health_issues" "Sonarr")
              (query "B" "radarr_system_health_issues" "Radarr")
              (query "C" "prowlarr_system_health_issues" "Prowlarr")
              (query "D" "bazarr_system_health_issues" "Bazarr")
            ]
          ))
          (at 12 1 6 4 (
            stat 106 "Download speed" "Bps" [
              (query "A" "qbittorrent_global_download_speed_bytes" "qBittorrent")
            ]
          ))
          (at 18 1 6 4 (
            stat 107 "Upload speed" "Bps" [ (query "A" "qbittorrent_global_upload_speed_bytes" "qBittorrent") ]
          ))
          (row 201 "qBittorrent" 5 false [ ])
          (at 0 6 16 8 (
            panel 8 "qBittorrent transfer rates" "Bps" [
              (query "A" "qbittorrent_global_download_speed_bytes" "download")
              (query "B" "qbittorrent_global_upload_speed_bytes" "upload")
            ]
          ))
          (at 16 6 8 8 (
            panel 108 "Torrent states" "short" [
              (query "A"
                "qbittorrent_torrent_states{name=~\"downloading|uploading|stalledDL|stalledUP|queuedDL|queuedUP|error|missingFiles|metaDL\"}"
                "{{name}}"
              )
            ]
          ))
          (at 0 14 6 4 (
            stat 9 "qBittorrent torrents and peers" "short" [
              (query "A" "qbittorrent_global_torrents" "torrents")
              (query "B" "qbittorrent_global_total_peer_connections" "peers")
            ]
          ))
          (at 6 14 6 4 (
            stat 109 "Download disk free" "bytes" [
              (query "A" "qbittorrent_global_free_space_on_disk_bytes" "Free")
            ]
          ))
          (at 12 14 6 4 (
            stat 110 "Session transferred" "bytes" [
              (query "A" "qbittorrent_global_session_downloaded_bytes" "Downloaded")
              (query "B" "qbittorrent_global_session_uploaded_bytes" "Uploaded")
            ]
          ))
          (recursiveUpdate (at 18 14 6 4 (
            stat 111 "Share ratio" "short" [ (query "A" "qbittorrent_global_ratio" "Ratio") ]
          )) { fieldConfig.defaults.decimals = 2; })
          (row 202 "Media queues & downloads" 18 false [ ])
          (at 0 19 6 4 (stat 10 "Sonarr queue" "short" [ (query "A" "sonarr_queue_total" "Queued") ]))
          (at 6 19 6 4 (stat 11 "Radarr queue" "short" [ (query "A" "radarr_queue_total" "Queued") ]))
          (at 12 19 6 4 (
            problems 12 "Unavailable indexers" [ (query "A" "prowlarr_indexer_unavailable" "{{indexer}}") ]
          ))
          (at 18 19 6 4 (
            stat 112 "Missing subtitles" "short" [
              (query "A" "bazarr_subtitles_missing_total" "Episodes")
              (query "B" "bazarr_movie_subtitles_missing_total" "Movies")
            ]
          ))
          (at 0 23 12 7 (
            panel 14 "slskd active transfer speeds" "Bps" [
              (query "A" "slskd_transfers_downloads_in_progress_speed_total_current" "download")
              (query "B" "slskd_transfers_uploads_in_progress_speed_total_current" "upload")
            ]
          ))
          (at 12 23 12 7 (
            panel 15 "slskd active files" "short" [
              (query "A" "slskd_transfers_downloads_in_progress_files_current" "downloading")
              (query "B" "slskd_transfers_uploads_in_progress_files_current" "uploading")
            ]
          ))
          (at 0 30 12 6 (
            panel 113 "slskd queued files" "short" [
              (query "A" "slskd_transfers_downloads_queued_files_current" "Download")
              (query "B" "slskd_transfers_uploads_queued_files_current" "Upload")
            ]
          ))
          (at 12 30 12 6 (
            stat 114 "Missing media" "short" [
              (query "A" "sonarr_episode_missing_total" "Episodes")
              (query "B" "radarr_movie_missing_total" "Movies")
            ]
          ))
          (row 203 "Availability" 36 false [ ])
          (at 0 37 14 9 gatusStatus)
          (at 14 37 10 9 (
            panel 7 "Gatus response time" "s" [
              (query "A" "gatus_results_duration_seconds" "{{group}} {{name}} · duration")
            ]
          ))
          (row 204 "Infrastructure" 47 true [
            (at 0 48 12 7 (
              panel 3 "Nginx requests / sec" "ops" [
                (query "A" "sum by (host, job) (rate(nginx_http_requests_total[$__rate_interval]))"
                  "{{host}} nginx · requests/s"
                )
              ]
            ))
            (at 12 48 12 7 (
              panel 4 "Nginx connections" "short" [
                (query "A" "nginx_connections_active" "{{host}} nginx · active")
                (query "B" "nginx_connections_reading" "{{host}} nginx · reading")
                (query "C" "nginx_connections_writing" "{{host}} nginx · writing")
                (query "D" "nginx_connections_waiting" "{{host}} nginx · waiting")
              ]
            ))
            (at 0 55 12 7 (
              panel 5 "DNS cache hits / misses" "ops" [
                (query "A" "rate(dnsmasq_hits[$__rate_interval])" "{{host}} dnsmasq · hits/s")
                (query "B" "rate(dnsmasq_misses[$__rate_interval])" "{{host}} dnsmasq · misses/s")
              ]
            ))
            (at 12 55 12 7 (
              panel 16 "TURN rate-limit buckets" "short" [
                (query "A" "turn_ratelimit_occupied_buckets" "{{host}} TURN · occupied")
                (query "B" "turn_ratelimit_total_buckets" "{{host}} TURN · total")
              ]
            ))
            (at 0 62 12 7 (
              panel 1 "PostgreSQL active connections" "short" [
                (query "A" "pg_stat_database_numbackends" "{{host}} {{datname}} · backends")
              ]
            ))
            (at 12 62 12 7 (
              panel 2 "PostgreSQL transactions / sec" "ops" [
                (query "A" "sum by (host, datname) (rate(pg_stat_database_xact_commit[$__rate_interval]))"
                  "{{host}} {{datname}} · commits/s"
                )
              ]
            ))
            (recursiveUpdate (at 0 69 6 4 (
              stat 101 "PostgreSQL availability" "short" [ (query "A" "pg_up" "{{host}}") ]
            )) { fieldConfig.defaults.mappings = statusMappings; })
            (recursiveUpdate (at 6 69 18 4 (
              stat 103 "Scrape targets" "short" [ (query "A" "up" "{{host}} {{job}} {{service}}") ]
            )) { fieldConfig.defaults.mappings = statusMappings; })
          ])
          (row 205 "Diagnostics & inventory" 74 true [
            (at 0 75 12 7 (
              panel 19 "Jellyfin process memory" "bytes" [
                (query "A" ''process_working_set_bytes{job="jellyfin"}'' "{{host}} Jellyfin")
              ]
            ))
            (at 12 75 12 7 (
              panel 20 "Jellyfin CPU" "percentunit" [
                (query "A" ''rate(process_cpu_seconds_total{job="jellyfin"}[$__rate_interval])''
                  "{{host}} Jellyfin"
                )
              ]
            ))
            (at 0 82 12 7 (
              panel 21 "Jellyfin requests by status" "ops" [
                (query "A" ''sum by (code) (rate(http_requests_received_total{job="jellyfin"}[$__rate_interval]))''
                  "HTTP {{code}}"
                )
              ]
            ))
            (at 12 82 12 7 (
              panel 22 "Jellyfin active HTTP requests" "short" [
                (query "A" ''sum(http_requests_in_progress{job="jellyfin"})'' "In progress")
              ]
            ))
            (at 0 89 12 7 (
              panel 23 "Jellyfin mean request duration" "s" [
                (query "A"
                  ''sum(rate(http_request_duration_seconds_sum{job="jellyfin"}[$__rate_interval])) / sum(rate(http_request_duration_seconds_count{job="jellyfin"}[$__rate_interval]))''
                  "Mean duration"
                )
              ]
            ))
            (at 12 89 12 7 (
              panel 17 "Forgejo repositories / users" "short" [
                (query "A" "gitea_repositories" "{{host}} Forgejo · repositories")
                (query "B" "gitea_users" "{{host}} Forgejo · users")
              ]
            ))
            (at 0 96 12 7 (
              panel 18 "Navidrome library objects" "short" [
                (query "A" ''db_model_totals{job="navidrome"}'' "{{host}} {{model}}")
              ]
            ))
            (at 12 96 12 7 (
              panel 13 "Bazarr missing / downloaded subtitles" "short" [
                (query "A" "bazarr_subtitles_missing_total" "{{host}} Bazarr · missing")
                (query "B" "bazarr_subtitles_downloaded_total" "{{host}} Bazarr · downloaded")
              ]
            ))
          ])
        ];
      });
    }
  ];
}
