{
  config,
  pkgs,
  ...
}: {
  server.domains = ["grafana.local"];
  services.nginx.virtualHosts."grafana.local" = {
    root = "/fake";
    locations = {
      "/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.grafana.settings.server.http_port}";
        proxyWebsockets = true;
        recommendedProxySettings = true;
      };
    };
  };

  meow.impermanence.directories = [
    {path = "/var/lib/grafana";}
  ];

  meow.sops.enableSecrets = ["server_grafana_radarr_api"];

  services.prometheus = {
    enable = true;
    # exporters = {
    #   exportarr-radarr = {
    #     url = "http://127.0.0.1:8787";
    #     apiKeyFile = config.sops.secrets."server_grafana_radarr_api".path;
    #   };
    # };

    scrapeConfigs = [
      {
        job_name = "watchtower";
        scrape_interval = "5s";
        metrics_path = "/v1/metrics";
        bearer_token = "token";
        static_configs = [
          {
            targets = [
              "127.0.0.1:8081"
            ];
          }
        ];
      }
    ];
  };

  services.grafana = {
    enable = true;
    settings = {
      server = {
        http_addr = "127.0.0.1";
        http_port = 3003;
        enable_gzip = true;
        enforce_domain = false;
      };

      analytics.reporting_enabled = false;
    };

    provision = {
      enable = true;
      dashboards.settings.providers = [
        {
          name = "Prometheus";
          type = "file";
          options.path = "/etc/grafana-dashboards";
        }
      ];

      datasources.settings.datasources = [
        {
          name = "Prometheus";
          type = "prometheus";
          isDefault = true;
          url = "http://127.0.0.1:${toString config.services.prometheus.port}";
        }
      ];
    };
  };

  environment.etc."grafana-dashboards/dashboard.json" = {
    source = ./dashboard.json;
    group = "grafana";
    user = "grafana";
  };
}
