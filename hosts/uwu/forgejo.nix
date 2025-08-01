{
  pkgs,
  lib,
  config,
  ...
}: {
  config = {
    server.domains = [
      "git.local"
      "git.home"
    ];

    services.nginx.virtualHosts."git.local" = {
      serverAliases = ["git.home"];
      locations = {
        "/" = {
          proxyPass = "http://127.0.0.1:${toString config.services.forgejo.settings.server.HTTP_PORT}";
          recommendedProxySettings = true;
          extraConfig = ''
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "Upgrade";
          '';
        };
      };
    };

    environment.systemPackages = let
      cfg = config.services.forgejo;
      forgejo-cli = pkgs.writeScriptBin "forgejo-cli" ''
        #!${pkgs.runtimeShell}
        cd ${cfg.stateDir}
        sudo=exec
        if [[ "$USER" != forgejo ]]; then
          sudo='exec /run/wrappers/bin/sudo -u ${cfg.user} -g ${cfg.group} --preserve-env=GITEA_WORK_DIR --preserve-env=GITEA_CUSTOM'
        fi
        # Note that these variable names will change
        export GITEA_WORK_DIR=${cfg.stateDir}
        export GITEA_CUSTOM=${cfg.customDir}
        $sudo ${lib.getExe cfg.package} "$@"
      '';
    in [
      forgejo-cli
    ];

    # services.gitea-actions-runner = {
    #   package = pkgs.forgejo-actions-runner;

    #   instances.default = {
    #     enable = true;
    #     name = "monolith";
    #     url = "https://git.example.com";
    #     # Obtaining the path to the runner token file may differ
    #     # tokenFile should be in format TOKEN=<secret>, since it's EnvironmentFile for systemd
    #     tokenFile = config.age.secrets.forgejo-runner-token.path;
    #     labels = [
    #       "ubuntu-25.04:docker://catthehacker/ubuntu:act-latest"
    #       ## optionally provide native execution on the host:
    #       # "native:host"
    #     ];
    #   };
    # };

    services.forgejo = {
      enable = true;
      stateDir = "/nix/persist/forgejo";

      lfs.enable = true;

      dump = {
        enable = true;
      };

      secrets = {
        # oauth2.JWT_SECRET = config.sops.secrets.forgejo_oauth_client_secret.path;
      };

      settings = {
        server = {
          ROOT_URL = "http://git.home";
          HTTP_PORT = 3001;
        };

        oauth2 = {
          ENABLED = true;
        };

        service.DISABLE_REGISTRATION = false;

        mailer = {
          ENABLED = false;
        };
      };

      database.type = "postgres";
    };
  };
}
