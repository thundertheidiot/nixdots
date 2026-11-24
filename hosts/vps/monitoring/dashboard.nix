{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (builtins) map toJSON listToAttrs;
  inherit (lib)
    recursiveUpdate
    head
    last
    filter
    foldl'
    max
    imap0
    imap1
    optionals
    nameValuePair
    optionalAttrs
    ;

  datasource = {
    type = "prometheus";
    uid = "\${datasource}";
  };
  query = refId: expr: legendFormat: { inherit refId expr legendFormat; };
  instant = map (
    target:
    target
    // {
      instant = true;
      range = false;
    }
  );

  # node_* is also exported by slskd, keep host panels on node_exporter
  node = ''job="node"'';
  fsFilter = ''${node},fstype!~"tmpfs|ramfs|squashfs|overlay|fuse.*"'';
  healthIssues = ''{__name__=~"(sonarr|radarr|prowlarr|bazarr)_system_health_issues"}'';

  thresholds = steps: {
    color.mode = "thresholds";
    thresholds = {
      mode = "absolute";
      steps = map (step: {
        color = head step;
        value = last step;
      }) steps;
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

  panel = title: unit: targets: {
    inherit title datasource targets;
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
        gradientMode = "opacity";
        axisBorderShow = false;
        showPoints = "never";
      };
    };
    fieldConfig.overrides =
      map
        (
          { regex, color }:
          {
            matcher = {
              id = "byRegexp";
              options = regex;
            };
            properties = [
              {
                id = "color";
                value = {
                  mode = "fixed";
                  fixedColor = color;
                };
              }
            ];
          }
        )
        [
          {
            regex = ".*[Dd]ownload.*";
            color = "blue";
          }
          {
            regex = ".*[Uu]pload.*";
            color = "purple";
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

  stat = title: unit: targets: {
    inherit title datasource;
    targets = instant targets;
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
  # counts that should be zero, coloured once they aren't
  alert =
    color: title: targets:
    recursiveUpdate (stat title "short" targets) {
      fieldConfig.defaults = {
        noValue = "0";
      }
      // thresholds [
        [
          "green"
          null
        ]
        [
          color
          1
        ]
      ];
    };
  problems = alert "red";
  updown =
    title: targets:
    recursiveUpdate (stat title "short" targets) { fieldConfig.defaults.mappings = statusMappings; };

  bars = title: unit: targets: {
    inherit title datasource;
    type = "bargauge";
    targets = instant targets;
    fieldConfig.defaults = {
      inherit unit;
      noValue = "No data";
      min = 0;
      decimals = if unit == "short" then 0 else 1;
      color = {
        mode = "fixed";
        fixedColor = "blue";
      };
    };
    options = {
      orientation = "horizontal";
      displayMode = "gradient";
      valueMode = "color";
      namePlacement = "left";
      showUnfilled = true;
      sizing = "manual";
      minVizHeight = 14;
      maxVizHeight = 24;
      text = {
        titleSize = 11;
        valueSize = 14;
      };
      reduceOptions = {
        calcs = [ "lastNotNull" ];
        values = false;
      };
    };
  };
  usage =
    title: targets:
    recursiveUpdate (bars title "percent" targets) {
      fieldConfig.defaults = {
        max = 100;
      }
      // thresholds [
        [
          "green"
          null
        ]
        [
          "orange"
          75
        ]
        [
          "red"
          90
        ]
      ];
    };

  table = title: columns: targets: {
    inherit title datasource;
    type = "table";
    targets = map (target: target // { format = "table"; }) (instant targets);
    fieldConfig.defaults = {
      noValue = "Nothing to report";
      custom = {
        wrapText = true;
        cellOptions.type = "auto";
      };
    };
    # every column but the last keeps a fixed width
    fieldConfig.overrides = map (column: {
      matcher = {
        id = "byName";
        options = column.label;
      };
      properties = [
        {
          id = "custom.width";
          value = column.width;
        }
      ];
    }) (filter (column: column ? width) columns);
    options.cellHeight = "sm";
    transformations = [
      {
        id = "filterFieldsByName";
        options.include.names = map (column: column.name) columns;
      }
      {
        id = "organize";
        options = {
          indexByName = listToAttrs (imap0 (i: column: nameValuePair column.name i) columns);
          renameByName = listToAttrs (map (column: nameValuePair column.name column.label) columns);
        };
      }
    ];
  };

  timeline = title: targets: {
    inherit title datasource targets;
    type = "state-timeline";
    fieldConfig.defaults = {
      noValue = "No data";
      mappings = statusMappings;
    }
    // thresholds [
      [
        "red"
        null
      ]
      [
        "green"
        1
      ]
    ];
    options = {
      mergeValues = true;
      showValue = "never";
      rowHeight = 0.8;
      legend.showLegend = false;
    };
  };

  # panels flow left to right in a 24 wide grid, wrapping like text
  size = w: h: panel: { inherit w h panel; };
  flow =
    y:
    foldl'
      (
        acc:
        {
          w,
          h,
          panel,
        }:
        let
          wrap = acc.x + w > 24;
          x = if wrap then 0 else acc.x;
          y = if wrap then acc.bottom else acc.y;
        in
        {
          x = x + w;
          inherit y;
          bottom = max acc.bottom (y + h);
          panels = acc.panels ++ [
            (
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
              }
            )
          ];
        }
      )
      {
        x = 0;
        inherit y;
        bottom = y;
        panels = [ ];
      };
  row = title: collapsed: items: { inherit title collapsed items; };
  layout =
    rows:
    imap1
      (
        i: panel:
        panel
        // {
          id = i;
        }
        // optionalAttrs (panel ? panels) {
          panels = imap1 (j: child: child // { id = i * 100 + j; }) panel.panels;
        }
      )
      (foldl'
        (
          acc:
          {
            title,
            collapsed,
            items,
          }:
          let
            inner = flow (acc.y + 1) items;
          in
          {
            y = if collapsed then acc.y + 1 else inner.bottom;
            panels =
              acc.panels
              ++ [
                {
                  inherit title collapsed;
                  type = "row";
                  gridPos = {
                    x = 0;
                    y = acc.y;
                    w = 24;
                    h = 1;
                  };
                  panels = optionals collapsed inner.panels;
                }
              ]
              ++ optionals (!collapsed) inner.panels;
          }
        )
        {
          y = 0;
          panels = [ ];
        }
        rows
      ).panels;

  rows = [
    (row "Overview" false [
      (size 4 4 (
        problems "Failing endpoints · 5m" [
          (query "A" "sum(1 - min_over_time(gatus_results_endpoint_success[5m]))" "Endpoints")
        ]
      ))
      (size 4 4 (problems "Scrape targets down" [ (query "A" "count(up == 0)" "Targets") ]))
      (size 4 4 (
        problems "Failed systemd units" [
          (query "A" ''sum(node_systemd_unit_state{${node},state="failed"})'' "Units")
        ]
      ))
      (size 4 4 (
        problems "Deployment failures" [
          (query "A" ''sum({__name__=~"comin_last_.*_failed|comin_is_suspended"})'' "Failures")
        ]
      ))
      (size 4 4 (
        alert "orange" "Media health warnings" [ (query "A" "count(${healthIssues} > 0)" "Warnings") ]
      ))
      (size 4 4 (alert "orange" "Reboots pending" [ (query "A" "sum(comin_need_to_reboot)" "Hosts") ]))

      (size 6 4 (
        usage "CPU busy" [
          (query "A"
            ''100 * (1 - avg by (host) (rate(node_cpu_seconds_total{${node},mode="idle"}[$__rate_interval])))''
            "{{host}}"
          )
        ]
      ))
      (size 6 4 (
        usage "Memory used" [
          (query "A"
            "100 * (1 - node_memory_MemAvailable_bytes{${node}} / node_memory_MemTotal_bytes{${node}})"
            "{{host}}"
          )
        ]
      ))
      (size 6 4 (
        usage "Fullest filesystem" [
          (query "A"
            "max by (host) (100 * (1 - node_filesystem_avail_bytes{${fsFilter}} / node_filesystem_size_bytes{${fsFilter}}))"
            "{{host}}"
          )
        ]
      ))
      (size 6 4 (stat "Uptime" "s" [ (query "A" "time() - node_boot_time_seconds{${node}}" "{{host}}") ]))

      (size 12 7 (
        timeline "Endpoint availability" [
          (query "A" "gatus_results_endpoint_success" "{{name}}")
        ]
      ))
      (size 12 7 (
        table "Media health messages"
          [
            {
              name = "service";
              label = "Service";
              width = 90;
            }
            {
              name = "type";
              label = "Level";
              width = 80;
            }
            {
              name = "message";
              label = "Message";
            }
          ]
          [ (query "A" "max by (service, type, message) (${healthIssues} > 0)" "") ]
      ))
    ])

    (row "Downloads & media" false [
      (size 14 8 (
        panel "qBittorrent transfer rates" "Bps" [
          (query "A" "qbittorrent_global_download_speed_bytes" "download")
          (query "B" "qbittorrent_global_upload_speed_bytes" "upload")
        ]
      ))
      (size 10 8 (
        bars "Torrent states" "short" [ (query "A" "qbittorrent_torrent_states > 0" "{{name}}") ]
      ))
      (size 6 4 (
        stat "Torrents / peers" "short" [
          (query "A" "qbittorrent_global_torrents" "Torrents")
          (query "B" "qbittorrent_global_total_peer_connections" "Peers")
        ]
      ))
      (size 6 4 (
        stat "Session transferred" "bytes" [
          (query "A" "qbittorrent_global_session_downloaded_bytes" "Downloaded")
          (query "B" "qbittorrent_global_session_uploaded_bytes" "Uploaded")
        ]
      ))
      (size 6 4 (
        recursiveUpdate (stat "Share ratio" "short" [ (query "A" "qbittorrent_global_ratio" "Ratio") ]) {
          fieldConfig.defaults.decimals = 2;
        }
      ))
      (size 6 4 (
        stat "Download disk free" "bytes" [
          (query "A" "qbittorrent_global_free_space_on_disk_bytes" "Free")
        ]
      ))
      (size 6 4 (
        stat "Arr queues" "short" [
          (query "A" "sonarr_queue_total" "Sonarr")
          (query "B" "radarr_queue_total" "Radarr")
        ]
      ))
      (size 6 4 (
        stat "Missing media" "short" [
          (query "A" "sonarr_episode_missing_total" "Episodes")
          (query "B" "radarr_movie_missing_total" "Movies")
        ]
      ))
      (size 6 4 (
        stat "Missing subtitles" "short" [
          (query "A" "bazarr_subtitles_missing_total" "Episodes")
          (query "B" "bazarr_movie_subtitles_missing_total" "Movies")
        ]
      ))
      (size 6 4 (
        problems "Unavailable indexers" [ (query "A" "prowlarr_indexer_unavailable" "{{indexer}}") ]
      ))
      (size 12 7 (
        panel "Soulseek transfer speeds" "Bps" [
          (query "A" "slskd_transfers_downloads_in_progress_speed_total_current" "download")
          (query "B" "slskd_transfers_uploads_in_progress_speed_total_current" "upload")
        ]
      ))
      (size 12 7 (
        panel "Soulseek files" "short" [
          (query "A" "slskd_transfers_downloads_in_progress_files_current" "download · active")
          (query "B" "slskd_transfers_downloads_queued_files_current" "download · queued")
          (query "C" "slskd_transfers_uploads_in_progress_files_current" "upload · active")
          (query "D" "slskd_transfers_uploads_queued_files_current" "upload · queued")
        ]
      ))
    ])

    (row "Library & discovery" true [
      (size 12 7 (
        panel "Media library size" "bytes" [
          (query "A" "sonarr_series_filesize_bytes" "TV")
          (query "B" "radarr_movie_filesize_total" "Movies")
        ]
      ))
      (size 12 7 (
        panel "Downloaded library items" "short" [
          (query "A" "sonarr_episode_downloaded_total" "Episodes")
          (query "B" "radarr_movie_downloaded_total" "Movies")
        ]
      ))
      (size 12 7 (
        panel "Indexer response time" "ms" [
          (query "A" "prowlarr_indexer_average_response_time_ms" "{{indexer}}")
        ]
      ))
      (size 12 7 (
        panel "Failed indexer searches · reported total" "short" [
          (query "A" "prowlarr_indexer_failed_queries_total" "{{indexer}}")
        ]
      ))
      (size 12 7 (
        panel "Soulseek incoming searches / second" "ops" [
          (query "A" "rate(slskd_search_incoming_requests_received[$__rate_interval])" "Received")
          (query "B" "rate(slskd_search_incoming_requests_dropped[$__rate_interval])" "Dropped")
        ]
      ))
      (size 12 7 (
        panel "Navidrome library objects" "short" [
          (query "A" ''db_model_totals{job="navidrome"}'' "{{model}}")
        ]
      ))
    ])

    (row "Host resources" true [
      (size 12 8 (
        panel "CPU busy" "percent" [
          (query "A"
            ''100 * (1 - avg by (host) (rate(node_cpu_seconds_total{${node},mode="idle"}[$__rate_interval])))''
            "{{host}}"
          )
        ]
      ))
      (size 12 8 (
        panel "Memory used" "percent" [
          (query "A"
            "100 * (1 - node_memory_MemAvailable_bytes{${node}} / node_memory_MemTotal_bytes{${node}})"
            "{{host}}"
          )
        ]
      ))
      (size 12 8 (
        panel "Pressure stall · time waiting" "percentunit" [
          (query "A" "rate(node_pressure_cpu_waiting_seconds_total{${node}}[$__rate_interval])"
            "{{host}} cpu"
          )
          (query "B" "rate(node_pressure_memory_waiting_seconds_total{${node}}[$__rate_interval])"
            "{{host}} memory"
          )
          (query "C" "rate(node_pressure_io_waiting_seconds_total{${node}}[$__rate_interval])" "{{host}} io")
        ]
      ))
      (size 12 8 (
        panel "Disk busy" "percentunit" [
          (query "A" ''rate(node_disk_io_time_seconds_total{${node},device!~"sr.*"}[$__rate_interval])''
            "{{host}} {{device}}"
          )
        ]
      ))
      (size 12 8 (
        panel "Network receive / second" "Bps" [
          (query "A" ''rate(node_network_receive_bytes_total{${node},device!~"lo|veth.*"}[$__rate_interval])''
            "{{host}} {{device}}"
          )
        ]
      ))
      (size 12 8 (
        panel "Network send / second" "Bps" [
          (query "A"
            ''rate(node_network_transmit_bytes_total{${node},device!~"lo|veth.*"}[$__rate_interval])''
            "{{host}} {{device}}"
          )
        ]
      ))
      (size 12 8 (
        usage "Filesystem used" [
          (query "A"
            "max by (host, device) (100 * (1 - node_filesystem_avail_bytes{${fsFilter}} / node_filesystem_size_bytes{${fsFilter}}))"
            "{{host}} {{device}}"
          )
        ]
      ))
      (size 12 8 (
        panel "Temperatures" "celsius" [
          (query "A" "max by (host, type) (node_thermal_zone_temp{${node}})" "{{host}} {{type}}")
        ]
      ))
    ])

    (row "Availability" true [
      (size 12 12 (
        panel "Gatus response time" "s" [ (query "A" "gatus_results_duration_seconds" "{{name}}") ]
      ))
      (size 12 12 (timeline "Scrape targets" [ (query "A" "up" "{{host}} {{job}} {{service}}") ]))
    ])

    (row "Services" true [
      (size 12 7 (
        panel "Connected XMPP clients" "short" [
          (query "A" "sum by (exported_host) (prosody_mod_c2s__connections)" "{{exported_host}}")
        ]
      ))
      (size 12 7 (
        panel "Incoming XMPP federation connections" "short" [
          (query "A" "sum by (type) (prosody_mod_s2s__connections_inbound)" "{{type}}")
        ]
      ))
      (size 12 7 (
        panel "Jellyfin requests by status" "ops" [
          (query "A" ''sum by (code) (rate(http_requests_received_total{job="jellyfin"}[$__rate_interval]))''
            "HTTP {{code}}"
          )
        ]
      ))
      (size 12 7 (
        panel "Jellyfin request load" "short" [
          (query "A" ''sum(http_requests_in_progress{job="jellyfin"})'' "In progress")
        ]
      ))
      (size 8 7 (
        panel "Jellyfin memory" "bytes" [
          (query "A" ''process_working_set_bytes{job="jellyfin"}'' "Working set")
        ]
      ))
      (size 8 7 (
        panel "Jellyfin CPU" "percentunit" [
          (query "A" ''rate(process_cpu_seconds_total{job="jellyfin"}[$__rate_interval])'' "CPU")
        ]
      ))
      (size 8 7 (
        panel "Jellyfin mean request duration" "s" [
          (query "A"
            ''sum(rate(http_request_duration_seconds_sum{job="jellyfin"}[$__rate_interval])) / sum(rate(http_request_duration_seconds_count{job="jellyfin"}[$__rate_interval]))''
            "Mean duration"
          )
        ]
      ))
      (size 6 4 (stat "Forgejo repositories" "short" [ (query "A" "gitea_repositories" "Repositories") ]))
      (size 6 4 (stat "Forgejo users" "short" [ (query "A" "gitea_users" "Users") ]))
    ])

    (row "Infrastructure" true [
      (size 12 7 (
        panel "Nginx requests / sec" "ops" [
          (query "A" "sum by (host) (rate(nginx_http_requests_total[$__rate_interval]))" "{{host}}")
        ]
      ))
      (size 12 7 (
        panel "Nginx connections" "short" [
          (query "A" "nginx_connections_active" "{{host}} active")
          (query "B" "nginx_connections_reading" "{{host}} reading")
          (query "C" "nginx_connections_writing" "{{host}} writing")
          (query "D" "nginx_connections_waiting" "{{host}} waiting")
        ]
      ))
      (size 12 7 (
        panel "DNS cache hits / misses" "ops" [
          (query "A" "rate(dnsmasq_hits[$__rate_interval])" "{{host}} hits")
          (query "B" "rate(dnsmasq_misses[$__rate_interval])" "{{host}} misses")
        ]
      ))
      (size 12 7 (
        panel "TURN rate-limit buckets" "short" [
          (query "A" "turn_ratelimit_occupied_buckets" "{{host}} occupied")
          (query "B" "turn_ratelimit_total_buckets" "{{host}} total")
        ]
      ))
      (size 12 7 (
        panel "PostgreSQL connections" "short" [
          (query "A" "sum by (host, datname) (pg_stat_database_numbackends) > 0" "{{host}} {{datname}}")
        ]
      ))
      (size 12 7 (
        panel "PostgreSQL commits / sec" "ops" [
          (query "A" "sum by (host, datname) (rate(pg_stat_database_xact_commit[$__rate_interval]))"
            "{{host}} {{datname}}"
          )
        ]
      ))
      (size 18 8 (
        bars "PostgreSQL database size" "bytes" [
          (query "A" ''pg_database_size_bytes{datname!~"template[01]"}'' "{{host}} {{datname}}")
        ]
      ))
      (size 6 8 (updown "PostgreSQL" [ (query "A" "pg_up" "{{host}}") ]))
    ])

    (row "Metrics forwarding" true [
      (size 12 7 (
        panel "Pending forwarding data" "bytes" [
          (query "A" "vmagent_remotewrite_pending_data_bytes" "{{url}}")
        ]
      ))
      (size 12 7 (
        panel "Dropped forwarding blocks / second" "ops" [
          (query "A" "rate(vmagent_remotewrite_packets_dropped_total[$__rate_interval])" "{{url}}")
        ]
      ))
    ])

    (row "Deployments" false [
      (size 12 6 (
        table "Deployed commits"
          [
            {
              name = "host";
              label = "Host";
              width = 120;
            }
            {
              name = "status";
              label = "Status";
              width = 100;
            }
            {
              name = "commit";
              label = "Commit";
            }
          ]
          [
            (query "A" ''label_replace(comin_deployment_info, "commit", "$1", "commit_id", "(.{10}).*")'' "")
          ]
      ))
      (size 12 6 (
        recursiveUpdate (problems "Comin failures" [
          (query "A" "comin_last_fetch_failed" "{{host}} fetch")
          (query "B" "comin_last_eval_failed" "{{host}} eval")
          (query "C" "comin_last_build_failed" "{{host}} build")
          (query "D" "comin_last_deployment_failed" "{{host}} deploy")
        ]) { options.text.valueSize = 28; }
      ))
      (size 12 4 (
        recursiveUpdate
          (stat "Deployments in range" "short" [
            (query "A"
              ''count by (host) (count by (host, commit_id) (count_over_time(comin_deployment_info{status="done"}[$__range])))''
              "{{host}}"
            )
          ])
          {
            fieldConfig.defaults.noValue = "0";
          }
      ))
      (size 12 4 (updown "Comin" [ (query "A" ''up{job="comin"}'' "{{host}}") ]))
    ])
  ];
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
        description = "Problems and host health first, media below, detail sections collapsed.";
        tags = [
          "services"
          "hosts"
        ];
        schemaVersion = 39;
        version = 5;
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
            inherit datasource;
            enable = true;
            expr = "changes(up[1m]) > 0";
            iconColor = "orange";
            titleFormat = "{{job}} {{instance}} changed state";
          }
          {
            name = "Deployments";
            inherit datasource;
            enable = true;
            expr = ''comin_deployment_info{status="done"} unless comin_deployment_info{status="done"} offset 2m'';
            step = "1m";
            iconColor = "blue";
            titleFormat = "{{host}} deployed {{commit_id}}";
          }
        ];
        panels = layout rows;
      });
    }
  ];
}
