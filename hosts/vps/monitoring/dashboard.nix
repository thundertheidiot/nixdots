{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (builtins) map toJSON;
  inherit (lib) recursiveUpdate head filter;
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
      max = if unit == "percent" then 100 else null;
      color.mode = "palette-classic";
      custom = {
        lineWidth = 2;
        fillOpacity = 12;
        axisBorderShow = false;
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
      tooltip = {
        mode = "multi";
        sort = "desc";
      };
      legend = {
        displayMode = "table";
        calcs = [
          "lastNotNull"
          "max"
        ];
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
  section = id: title: y: {
    inherit id;
    title = "";
    type = "text";
    options = {
      mode = "markdown";
      content = "## ${title}";
    };
    transparent = true;
    gridPos = {
      x = 0;
      inherit y;
      w = 24;
      h = 2;
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
  config.services.grafana.settings.dashboards.default_home_dashboard_path = "${
    (head (
      filter (
        provider: provider.name == "services"
      ) config.services.grafana.provision.dashboards.settings.providers
    )).options.path
  }/services.json";

  config.services.grafana.provision.dashboards.settings.providers = [
    {
      name = "services";
      options.path = pkgs.writeTextDir "services.json" (toJSON {
        uid = "vps-services";
        title = "Services & hosts";
        description = "Service health, media activity and availability first; infrastructure, host resources and diagnostics below.";
        tags = [
          "services"
          "hosts"
        ];
        schemaVersion = 39;
        version = 4;
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
          (section 200 "Overview" 0)
          (at 0 2 6 4 (
            problems 104 "Failing endpoints · last 5m" [
              (query "A" "sum(1 - min_over_time(gatus_results_endpoint_success[5m]))" "Endpoints")
            ]
          ))
          (at 6 2 6 4 (
            problems 105 "Media health issues" [
              (query "A" "sonarr_system_health_issues" "Sonarr")
              (query "B" "radarr_system_health_issues" "Radarr")
              (query "C" "prowlarr_system_health_issues" "Prowlarr")
              (query "D" "bazarr_system_health_issues" "Bazarr")
            ]
          ))
          (at 12 2 6 4 (
            stat 106 "Download speed" "Bps" [
              (query "A" "qbittorrent_global_download_speed_bytes" "qBittorrent")
            ]
          ))
          (at 18 2 6 4 (
            stat 107 "Upload speed" "Bps" [ (query "A" "qbittorrent_global_upload_speed_bytes" "qBittorrent") ]
          ))
          (section 201 "qBittorrent" 6)
          (at 0 8 16 8 (
            panel 8 "qBittorrent transfer rates" "Bps" [
              (query "A" "qbittorrent_global_download_speed_bytes" "download")
              (query "B" "qbittorrent_global_upload_speed_bytes" "upload")
            ]
          ))
          (at 16 8 8 8 (
            panel 108 "Torrent states" "short" [
              (query "A"
                "qbittorrent_torrent_states{name=~\"downloading|uploading|stalledDL|stalledUP|queuedDL|queuedUP|error|missingFiles|metaDL\"}"
                "{{name}}"
              )
            ]
          ))
          (at 0 16 6 4 (
            stat 9 "qBittorrent torrents and peers" "short" [
              (query "A" "qbittorrent_global_torrents" "torrents")
              (query "B" "qbittorrent_global_total_peer_connections" "peers")
            ]
          ))
          (at 6 16 6 4 (
            stat 109 "Download disk free" "bytes" [
              (query "A" "qbittorrent_global_free_space_on_disk_bytes" "Free")
            ]
          ))
          (at 12 16 6 4 (
            stat 110 "Session transferred" "bytes" [
              (query "A" "qbittorrent_global_session_downloaded_bytes" "Downloaded")
              (query "B" "qbittorrent_global_session_uploaded_bytes" "Uploaded")
            ]
          ))
          (recursiveUpdate (at 18 16 6 4 (
            stat 111 "Share ratio" "short" [ (query "A" "qbittorrent_global_ratio" "Ratio") ]
          )) { fieldConfig.defaults.decimals = 2; })
          (section 202 "Media queues & downloads" 20)
          (at 0 22 6 4 (stat 10 "Sonarr queue" "short" [ (query "A" "sonarr_queue_total" "Queued") ]))
          (at 6 22 6 4 (stat 11 "Radarr queue" "short" [ (query "A" "radarr_queue_total" "Queued") ]))
          (at 12 22 6 4 (
            problems 12 "Unavailable indexers" [ (query "A" "prowlarr_indexer_unavailable" "{{indexer}}") ]
          ))
          (at 18 22 6 4 (
            stat 112 "Missing subtitles" "short" [
              (query "A" "bazarr_subtitles_missing_total" "Episodes")
              (query "B" "bazarr_movie_subtitles_missing_total" "Movies")
            ]
          ))
          (at 0 26 12 7 (
            panel 14 "slskd active transfer speeds" "Bps" [
              (query "A" "slskd_transfers_downloads_in_progress_speed_total_current" "download")
              (query "B" "slskd_transfers_uploads_in_progress_speed_total_current" "upload")
            ]
          ))
          (at 12 26 12 7 (
            panel 15 "slskd active files" "short" [
              (query "A" "slskd_transfers_downloads_in_progress_files_current" "downloading")
              (query "B" "slskd_transfers_uploads_in_progress_files_current" "uploading")
            ]
          ))
          (at 0 33 12 6 (
            panel 113 "slskd queued files" "short" [
              (query "A" "slskd_transfers_downloads_queued_files_current" "Download")
              (query "B" "slskd_transfers_uploads_queued_files_current" "Upload")
            ]
          ))
          (at 12 33 12 6 (
            stat 114 "Missing media" "short" [
              (query "A" "sonarr_episode_missing_total" "Episodes")
              (query "B" "radarr_movie_missing_total" "Movies")
            ]
          ))
          (section 208 "Library & discovery" 39)
          (at 0 41 12 7 (
            panel 401 "Media library size" "bytes" [
              (query "A" "sonarr_series_filesize_bytes" "{{host}} TV")
              (query "B" "radarr_movie_filesize_total" "{{host}} Movies")
            ]
          ))
          (at 12 41 12 7 (
            panel 402 "Downloaded library items" "short" [
              (query "A" "sonarr_episode_downloaded_total" "Episodes")
              (query "B" "radarr_movie_downloaded_total" "Movies")
            ]
          ))
          (at 0 48 12 7 (
            panel 403 "Indexer response time" "ms" [
              (query "A" "prowlarr_indexer_average_response_time_ms" "{{indexer}}")
            ]
          ))
          (at 12 48 12 7 (
            panel 404 "Failed indexer searches · reported total" "short" [
              (query "A" "prowlarr_indexer_failed_queries_total" "{{indexer}}")
            ]
          ))
          (at 0 55 24 7 (
            panel 405 "Soulseek incoming searches / second" "ops" [
              (query "A" "rate(slskd_search_incoming_requests_received[$__rate_interval])" "Received")
              (query "B" "rate(slskd_search_incoming_requests_dropped[$__rate_interval])" "Dropped")
            ]
          ))
          (section 209 "XMPP activity" 62)
          (at 0 64 12 7 (
            panel 406 "Connected XMPP clients" "short" [
              (query "A" "sum by (host, exported_host) (prosody_mod_c2s__connections)" "{{exported_host}}")
            ]
          ))
          (at 12 64 12 7 (
            panel 407 "Incoming XMPP federation connections" "short" [
              (query "A" "sum by (host, type) (prosody_mod_s2s__connections_inbound)" "{{host}} {{type}}")
            ]
          ))
          (section 203 "Availability" 73)
          (at 0 75 14 9 gatusStatus)
          (at 14 75 10 9 (
            panel 7 "Gatus response time" "s" [
              (query "A" "gatus_results_duration_seconds" "{{group}} {{name}} · duration")
            ]
          ))
          (section 204 "Infrastructure" 85)
          (at 0 87 12 7 (
            panel 3 "Nginx requests / sec" "ops" [
              (query "A" "sum by (host, job) (rate(nginx_http_requests_total[$__rate_interval]))"
                "{{host}} nginx · requests/s"
              )
            ]
          ))
          (at 12 87 12 7 (
            panel 4 "Nginx connections" "short" [
              (query "A" "nginx_connections_active" "{{host}} nginx · active")
              (query "B" "nginx_connections_reading" "{{host}} nginx · reading")
              (query "C" "nginx_connections_writing" "{{host}} nginx · writing")
              (query "D" "nginx_connections_waiting" "{{host}} nginx · waiting")
            ]
          ))
          (at 0 94 12 7 (
            panel 5 "DNS cache hits / misses" "ops" [
              (query "A" "rate(dnsmasq_hits[$__rate_interval])" "{{host}} dnsmasq · hits/s")
              (query "B" "rate(dnsmasq_misses[$__rate_interval])" "{{host}} dnsmasq · misses/s")
            ]
          ))
          (at 12 94 12 7 (
            panel 16 "TURN rate-limit buckets" "short" [
              (query "A" "turn_ratelimit_occupied_buckets" "{{host}} TURN · occupied")
              (query "B" "turn_ratelimit_total_buckets" "{{host}} TURN · total")
            ]
          ))
          (at 0 101 12 7 (
            panel 1 "PostgreSQL active connections" "short" [
              (query "A" "pg_stat_database_numbackends" "{{host}} {{datname}} · backends")
            ]
          ))
          (at 12 101 12 7 (
            panel 2 "PostgreSQL transactions / sec" "ops" [
              (query "A" "sum by (host, datname) (rate(pg_stat_database_xact_commit[$__rate_interval]))"
                "{{host}} {{datname}} · commits/s"
              )
            ]
          ))
          (at 0 108 24 8 (
            panel 408 "PostgreSQL database size" "bytes" [
              (query "A" ''pg_database_size_bytes{datname!~"template[01]"}'' "{{host}} {{datname}}")
            ]
          ))
          (recursiveUpdate (at 0 116 6 4 (
            stat 101 "PostgreSQL availability" "short" [ (query "A" "pg_up" "{{host}}") ]
          )) { fieldConfig.defaults.mappings = statusMappings; })
          (recursiveUpdate (at 6 116 18 4 (
            stat 103 "Scrape targets" "short" [ (query "A" "up" "{{host}} {{job}} {{service}}") ]
          )) { fieldConfig.defaults.mappings = statusMappings; })

          (section 206 "Host resources" 121)
          (at 0 123 12 8 (
            panel 301 "CPU busy" "percent" [
              (query "A"
                ''100 * (1 - avg by (host, instance) (rate(node_cpu_seconds_total{mode="idle"}[$__rate_interval])))''
                "{{host}} {{instance}}"
              )
            ]
          ))
          (at 12 123 12 8 (
            panel 302 "Memory used" "percent" [
              (query "A" "100 * (1 - node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes)"
                "{{host}} {{instance}}"
              )
            ]
          ))
          (at 0 131 24 16 (
            panel 303 "Filesystem used" "percent" [
              (query "A"
                ''topk by (host, device) (1, 100 * (1 - node_filesystem_avail_bytes{fstype!~"tmpfs|ramfs|squashfs|overlay"} / node_filesystem_size_bytes))''
                "{{host}} {{device}}"
              )
            ]
          ))
          (at 0 139 12 16 (
            panel 304 "Network receive / second" "Bps" [
              (query "A" ''rate(node_network_receive_bytes_total{device!="lo"}[$__rate_interval])''
                "{{host}} {{device}}"
              )
            ]
          ))
          (at 12 139 12 16 (
            panel 309 "Network send / second" "Bps" [
              (query "A" ''rate(node_network_transmit_bytes_total{device!="lo"}[$__rate_interval])''
                "{{host}} {{device}}"
              )
            ]
          ))
          (at 0 153 12 8 (
            panel 305 "Failed systemd units" "short" [
              (query "A" ''sum by (host, instance) (node_systemd_unit_state{state="failed"})''
                "{{host}} {{instance}}"
              )
            ]
          ))
          (at 12 153 12 8 (
            panel 306 "Scrape health · history" "short" [
              (query "A" "up" "{{host}} {{job}} {{instance}}")
            ]
          ))
          (section 207 "Metrics forwarding" 161)
          (at 0 163 12 8 (
            panel 307 "Pending forwarding data" "bytes" [
              (query "A" "vmagent_remotewrite_pending_data_bytes" "{{host}} {{instance}}")
            ]
          ))
          (at 12 163 12 8 (
            panel 308 "Dropped forwarding blocks / second" "ops" [
              (query "A" "rate(vmagent_remotewrite_packets_dropped_total[$__rate_interval])"
                "{{host}} {{instance}}"
              )
            ]
          ))

          (section 205 "Diagnostics & inventory" 172)
          (at 0 174 12 7 (
            panel 19 "Jellyfin process memory" "bytes" [
              (query "A" ''process_working_set_bytes{job="jellyfin"}'' "{{host}} Jellyfin")
            ]
          ))
          (at 12 174 12 7 (
            panel 20 "Jellyfin CPU" "percentunit" [
              (query "A" ''rate(process_cpu_seconds_total{job="jellyfin"}[$__rate_interval])''
                "{{host}} Jellyfin"
              )
            ]
          ))
          (at 0 181 12 7 (
            panel 21 "Jellyfin requests by status" "ops" [
              (query "A" ''sum by (code) (rate(http_requests_received_total{job="jellyfin"}[$__rate_interval]))''
                "HTTP {{code}}"
              )
            ]
          ))
          (at 12 181 12 7 (
            panel 22 "Jellyfin active HTTP requests" "short" [
              (query "A" ''sum(http_requests_in_progress{job="jellyfin"})'' "In progress")
            ]
          ))
          (at 0 188 12 7 (
            panel 23 "Jellyfin mean request duration" "s" [
              (query "A"
                ''sum(rate(http_request_duration_seconds_sum{job="jellyfin"}[$__rate_interval])) / sum(rate(http_request_duration_seconds_count{job="jellyfin"}[$__rate_interval]))''
                "Mean duration"
              )
            ]
          ))
          (at 12 188 12 7 (
            panel 17 "Forgejo repositories / users" "short" [
              (query "A" "gitea_repositories" "{{host}} Forgejo · repositories")
              (query "B" "gitea_users" "{{host}} Forgejo · users")
            ]
          ))
          (at 0 195 12 7 (
            panel 18 "Navidrome library objects" "short" [
              (query "A" ''db_model_totals{job="navidrome"}'' "{{host}} {{model}}")
            ]
          ))
          (at 12 195 12 7 (
            panel 13 "Bazarr missing / downloaded subtitles" "short" [
              (query "A" "bazarr_subtitles_missing_total" "{{host}} Bazarr · missing")
              (query "B" "bazarr_subtitles_downloaded_total" "{{host}} Bazarr · downloaded")
            ]
          ))

          (section 208 "Comin deployments" 203)
          (recursiveUpdate (at 0 205 6 5 (
            stat 410 "Comin availability" "short" [
              (query "A" ''up{job="comin"}'' "{{host}}")
            ]
          )) { fieldConfig.defaults.mappings = statusMappings; })
          (at 6 205 9 5 (
            problems 411 "Last Comin failures" [
              (query "A" "comin_last_fetch_failed" "{{host}} {{remote_name}} · fetch")
              (query "B" "comin_last_eval_failed" "{{host}} · evaluation")
              (query "C" "comin_last_build_failed" "{{host}} · build")
              (query "D" "comin_last_deployment_failed" "{{host}} · deployment")
            ]
          ))
          (at 15 205 9 5 (
            problems 412 "Comin attention" [
              (query "A" "comin_is_suspended" "{{host}} · suspended")
              (query "B" "comin_need_to_reboot" "{{host}} · reboot required")
            ]
          ))
          (at 0 210 24 5 (
            stat 413 "Last Comin deployment" "short" [
              (query "A" "comin_deployment_info" "{{host}} · {{status}} · {{commit_id}}")
            ]
          ))
          (at 0 215 24 5 (
            stat 414 "Comin deployments in range" "short" [
              (query "A" ''count by (host) (count_over_time(comin_deployment_info{status="done"}[$__range]))'' "{{host}} · succeeded")
              (query "B" ''count by (host) (count_over_time(comin_deployment_info{status="failed"}[$__range]))'' "{{host}} · failed")
            ]
          ))

        ];
      });
    }
  ];
}
