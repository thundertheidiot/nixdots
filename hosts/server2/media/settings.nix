{ config, ... }:
{
  imports = [
    ./secrets.nix
    ./arr-extra.nix
  ];

  # Application settings; credentials are runtime SOPS references.
  config.nixflix = {
    prowlarr = {
      config = {
        # Keep both app registrations, adding Other for LimeTorrents' keywordless feed.
        applications =
          builtins.map
            (
              serviceName:
              let
                service = config.nixflix.${serviceName};
                name = if serviceName == "radarr" then "Radarr" else "Sonarr";
                url =
                  app:
                  "http://${app.connectionAddress}:${toString app.config.hostConfig.port}${app.config.hostConfig.urlBase}";
              in
              {
                inherit name;
                implementationName = name;
                apiKey = service.config.apiKey;
                baseUrl = url service;
                prowlarrUrl = url config.nixflix.prowlarr;
              }
              // (
                if serviceName == "radarr" then
                  {
                    syncCategories = [
                      2000
                      2010
                      2020
                      2030
                      2040
                      2045
                      2050
                      2060
                      2070
                      2080
                      2090
                      8000
                    ];
                  }
                else
                  { }
              )
            )
            [
              "radarr"
              "sonarr"
            ];
        indexers = [
          {
            name = "1337x";
            schemaName = "1337x";
            tags = [
              "cloudflare"
            ];
            appProfileId = 1;
            definitionFile = "1337x";
            baseUrl = "https://1337x.to/";
            "baseSettings.limitsUnit" = 0;
            "torrentBaseSettings.preferMagnetUrl" = false;
            uploader = "";
            primarydownloadlink = 1;
            fallbackdownloadlink = 0;
            disablesort = false;
            sort = 2;
            type = 1;
            enable = false;
            priority = 25;
          }
          {
            name = "kickasstorrents.ws";
            schemaName = "kickasstorrents.ws";
            tags = [ ];
            appProfileId = 1;
            definitionFile = "kickasstorrents-ws";
            baseUrl = "https://kickasstorrents.bz/";
            "baseSettings.limitsUnit" = 0;
            "torrentBaseSettings.preferMagnetUrl" = false;
            sort = 2;
            type = 1;
            enable = true;
            priority = 25;
          }
          {
            name = "Knaben";
            schemaName = "Knaben";
            tags = [ ];
            appProfileId = 1;
            "baseSettings.limitsUnit" = 0;
            "torrentBaseSettings.preferMagnetUrl" = false;
            enable = true;
            priority = 25;
          }
          {
            name = "LimeTorrents";
            schemaName = "LimeTorrents";
            tags = [ ];
            appProfileId = 1;
            definitionFile = "limetorrents";
            "baseSettings.limitsUnit" = 0;
            "torrentBaseSettings.preferMagnetUrl" = false;
            primarydownloadlink = 1;
            fallbackdownloadlink = 0;
            sort = 0;
            enable = true;
            priority = 25;
          }
          {
            name = "Magnet Cat";
            schemaName = "Magnet Cat";
            tags = [ ];
            appProfileId = 1;
            definitionFile = "magnetcat";
            baseUrl = "https://clmclm.com/";
            "baseSettings.limitsUnit" = 0;
            "torrentBaseSettings.preferMagnetUrl" = false;
            cat-id = 0;
            sort = 2;
            enable = true;
            priority = 25;
          }
          {
            name = "Nyaa.si";
            schemaName = "Nyaa.si";
            tags = [ ];
            appProfileId = 1;
            definitionFile = "nyaasi";
            baseUrl = "https://nyaa.mom/";
            "baseSettings.limitsUnit" = 0;
            "torrentBaseSettings.preferMagnetUrl" = false;
            prefer_magnet_links = true;
            sonarr_compatibility = false;
            strip_s01 = false;
            radarr_compatibility = false;
            filter-id = 0;
            cat-id = 0;
            sort = 0;
            type = 1;
            enable = true;
            priority = 25;
          }
          {
            name = "Rastastugan (API)";
            schemaName = "Rastastugan (API)";
            tags = [ ];
            appProfileId = 1;
            definitionFile = "rastastugan-api";
            baseUrl = "https://rastastugan.org/";
            "baseSettings.limitsUnit" = 0;
            "torrentBaseSettings.seedRatio" = 3;
            "torrentBaseSettings.preferMagnetUrl" = false;
            apikey = {
              _secret = "/run/media-stack/secrets/indexer-rastastugan-api-apikey";
            };
            freeleech = false;
            single_file_release_use_filename = true;
            sort = 0;
            type = 1;
            enable = false;
            priority = 25;
          }
          {
            name = "SceneTime";
            schemaName = "SceneTime (API)";
            definitionFile = "scenetime-api";
            tags = [ ];
            appProfileId = 1;
            baseUrl = "https://www.scenetime.com/";
            apikey = {
              _secret = "/run/media-stack/secrets/indexer-scenetime-token";
            };
            freeleech = false;
            "baseSettings.limitsUnit" = 0;
            "torrentBaseSettings.seedRatio" = 3;
            "torrentBaseSettings.preferMagnetUrl" = false;
            enable = false;
            priority = 25;
          }
          {
            name = "TorrentDownload";
            schemaName = "TorrentDownload";
            tags = [ ];
            appProfileId = 1;
            definitionFile = "torrentdownload";
            "baseSettings.limitsUnit" = 0;
            "torrentBaseSettings.preferMagnetUrl" = false;
            sort = 1;
            enable = true;
            priority = 25;
          }
          {
            name = "YTS";
            schemaName = "YTS";
            tags = [ ];
            appProfileId = 1;
            definitionFile = "yts";
            baseUrl = "https://yts.gg/";
            "baseSettings.limitsUnit" = 0;
            "torrentBaseSettings.preferMagnetUrl" = false;
            apiurl = "movies-api.accel.li";
            enable = true;
            priority = 25;
          }
        ];
        apiKey = {
          _secret = "/run/media-stack/secrets/prowlarr-api-key";
        };
        hostConfig = {
          port = 9696;
          sslPort = 6969;
          enableSsl = false;
          launchBrowser = true;
          authenticationMethod = "external";
          authenticationRequired = "enabled";
          analyticsEnabled = true;
          logLevel = "debug";
          logSizeLimit = 1;
          consoleLogLevel = "";
          branch = "master";
          sslCertPath = "";
          urlBase = "";
          instanceName = "Prowlarr";
          applicationUrl = "";
          updateAutomatically = false;
          updateMechanism = "external";
          updateScriptPath = "";
          proxyEnabled = false;
          proxyType = "http";
          proxyHostname = "";
          proxyPort = 8080;
          proxyBypassFilter = "";
          proxyBypassLocalAddresses = true;
          certificateValidation = "enabled";
          backupFolder = "Backups";
          backupInterval = 7;
          backupRetention = 28;
          trustCgnatIpAddresses = false;
          username = "admin";
          password = {
            _secret = "/run/media-stack/secrets/prowlarr-ui-password";
          };
        };
      };
    };
    slskd = {
      settings = {
        shares = {
          directories = [
            "/media/music"
            "/media/metal"
            "/media/breakcore"
          ];
        };
        transfers = {
          upload = {
            speed_limit = 1000;
          };
        };
        soulseek = {
          username = {
            _secret = "/run/media-stack/secrets/slskd-soulseek-username";
          };
          password = {
            _secret = "/run/media-stack/secrets/slskd-soulseek-password";
          };
        };
      };
      username = {
        _secret = "/run/media-stack/secrets/slskd-username";
      };
      password = {
        _secret = "/run/media-stack/secrets/slskd-password";
      };
    };
    radarr = {
      config = {
        apiKey = {
          _secret = "/run/media-stack/secrets/radarr-api-key";
        };
        hostConfig = {
          port = 7878;
          sslPort = 9898;
          enableSsl = false;
          launchBrowser = true;
          authenticationMethod = "external";
          authenticationRequired = "enabled";
          analyticsEnabled = true;
          logLevel = "debug";
          logSizeLimit = 1;
          consoleLogLevel = "";
          branch = "master";
          sslCertPath = "";
          urlBase = "";
          instanceName = "Radarr";
          applicationUrl = "";
          updateAutomatically = false;
          updateMechanism = "external";
          updateScriptPath = "";
          proxyEnabled = false;
          proxyType = "http";
          proxyHostname = "";
          proxyPort = 8080;
          proxyBypassFilter = "";
          proxyBypassLocalAddresses = true;
          certificateValidation = "enabled";
          backupFolder = "Backups";
          backupInterval = 7;
          backupRetention = 28;
          trustCgnatIpAddresses = false;
          username = "admin";
          password = {
            _secret = "/run/media-stack/secrets/radarr-ui-password";
          };
        };
        rootFolders = [
          {
            path = "/media/movies";
          }
          {
            path = "/media/movies_2";
          }
        ];
        mediaManagement = {
          autoUnmonitorPreviouslyDownloadedMovies = false;
          recycleBin = "";
          recycleBinCleanupDays = 7;
          downloadPropersAndRepacks = "preferAndUpgrade";
          createEmptyMovieFolders = false;
          deleteEmptyFolders = false;
          fileDate = "none";
          rescanAfterRefresh = "always";
          setPermissionsLinux = false;
          chmodFolder = "755";
          chownGroup = "";
          skipFreeSpaceCheckWhenImporting = false;
          minimumFreeSpaceWhenImporting = 100;
          copyUsingHardlinks = true;
          useScriptImport = false;
          scriptImportPath = "";
          importExtraFiles = true;
          extraFileExtensions = "srt";
          enableMediaInfo = true;
        };
        delayProfiles = [
          {
            enableUsenet = true;
            enableTorrent = true;
            preferredProtocol = "usenet";
            usenetDelay = 0;
            torrentDelay = 0;
            bypassIfHighestQuality = true;
            bypassIfAboveCustomFormatScore = false;
            minimumCustomFormatScore = 0;
            order = 2147483647;
            tags = [ ];
            id = 1;
          }
        ];
      };
      mediaDirs = [
        "/media/movies"
        "/media/movies_2"
      ];
    };
    sonarr = {
      config = {
        apiKey = {
          _secret = "/run/media-stack/secrets/sonarr-api-key";
        };
        hostConfig = {
          port = 8989;
          sslPort = 9898;
          enableSsl = false;
          launchBrowser = true;
          authenticationMethod = "external";
          authenticationRequired = "enabled";
          analyticsEnabled = true;
          logLevel = "debug";
          logSizeLimit = 1;
          consoleLogLevel = "";
          branch = "main";
          sslCertPath = "";
          urlBase = "";
          instanceName = "Sonarr";
          applicationUrl = "";
          updateAutomatically = false;
          updateMechanism = "external";
          updateScriptPath = "";
          proxyEnabled = false;
          proxyType = "http";
          proxyHostname = "";
          proxyPort = 8080;
          proxyBypassFilter = "";
          proxyBypassLocalAddresses = true;
          certificateValidation = "enabled";
          backupFolder = "Backups";
          backupInterval = 7;
          backupRetention = 28;
          trustCgnatIpAddresses = false;
          username = "admin";
          password = {
            _secret = "/run/media-stack/secrets/sonarr-ui-password";
          };
        };
        rootFolders = [
          {
            path = "/media/series";
          }
          {
            path = "/media/anime";
          }
          {
            path = "/media/series_2";
          }
        ];
        mediaManagement = {
          autoUnmonitorPreviouslyDownloadedEpisodes = false;
          recycleBin = "";
          recycleBinCleanupDays = 7;
          downloadPropersAndRepacks = "preferAndUpgrade";
          createEmptySeriesFolders = false;
          deleteEmptyFolders = false;
          fileDate = "none";
          rescanAfterRefresh = "always";
          setPermissionsLinux = false;
          chmodFolder = "755";
          chownGroup = "";
          episodeTitleRequired = "always";
          skipFreeSpaceCheckWhenImporting = false;
          minimumFreeSpaceWhenImporting = 100;
          copyUsingHardlinks = true;
          useScriptImport = false;
          scriptImportPath = "";
          importExtraFiles = false;
          extraFileExtensions = "srt";
          enableMediaInfo = true;
        };
        delayProfiles = [
          {
            enableUsenet = true;
            enableTorrent = true;
            preferredProtocol = "usenet";
            usenetDelay = 0;
            torrentDelay = 0;
            bypassIfHighestQuality = true;
            bypassIfAboveCustomFormatScore = false;
            minimumCustomFormatScore = 0;
            order = 2147483647;
            tags = [ ];
            id = 1;
          }
        ];
      };
      mediaDirs = [
        "/media/series"
        "/media/anime"
        "/media/series_2"
      ];
    };
    downloadarr = {
      enable = true;
      qbittorrent = {
        username = {
          _secret = "/run/media-stack/secrets/radarr-qbit-username";
        };
        password = {
          _secret = "/run/media-stack/secrets/radarr-qbit-password";
        };
        categories = {
          radarr = "radarr";
          sonarr = "tv-sonarr";
          lidarr = "lidarr";
        };
      };
    };
  };
}
