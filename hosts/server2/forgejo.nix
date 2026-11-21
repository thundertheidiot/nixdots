{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) getExe getExe' mkAfter;
  nya = config.meow.dns.domain;
in
{
  services.nginx.virtualHosts."git.${nya}" = {
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString config.services.forgejo.settings.server.HTTP_PORT}";
      recommendedProxySettings = true;
      proxyWebsockets = true;
    };
  };

  meow.impermanence.directories = [
    {
      path = "/var/lib/forgejo";
      persistPath = "${config.meow.impermanence.persist}/forgejo";
      user = "forgejo";
      group = "forgejo";
    }
    {
      path = "/var/lib/postgresql";
      user = "postgres";
      group = "postgres";
      permissions = "0700";
    }
  ];

  services.postgresql = {
    enable = true;
    ensureDatabases = [ "forgejo" ];
    ensureUsers = [
      {
        name = "forgejo";
        ensureDBOwnership = true;
      }
    ];
    authentication = lib.mkOverride 10 ''
      #type database  DBuser  auth-method
      local all postgres peer
      local all prometheus-postgres-exporter peer
      local sameuser all peer
    '';
  };

  services.forgejo = {
    enable = true;
    lfs.enable = true;
    database = {
      type = "postgres";
      host = "/run/postgresql";
      name = "forgejo";
      user = "forgejo";
    };
    settings.server = {
      ROOT_URL = "https://git.${nya}/";
      HTTP_ADDR = "127.0.0.1";
      HTTP_PORT = 3001;
    };
  };

  meow.sops.enableSecrets = [ "kanidm_forgejo" ];
  sops.secrets.kanidm_forgejo.restartUnits = [ "forgejo.service" ];

  services.kanidm.provision.systems.oauth2.forgejo = {
    displayName = "Forgejo";
    originUrl = "https://git.${nya}/user/oauth2/kanidm/callback";
    originLanding = "https://git.${nya}/";
    basicSecretFile = config.sops.secrets.kanidm_forgejo.path;
    preferShortUsername = true;
    # forgejo doesn't do pkce as a client
    allowInsecureClientDisablePkce = true;
    scopeMaps.admins = [
      "openid"
      "email"
      "profile"
    ];
  };

  systemd.services.forgejo = {
    serviceConfig.LoadCredential = [ "kanidm:${config.sops.secrets.kanidm_forgejo.path}" ];

    # login source lives in the db, sync it before the server loads its providers
    preStart = mkAfter ''
      args=(
        --name kanidm
        --provider openidConnect
        --key forgejo
        --secret "$(< "$CREDENTIALS_DIRECTORY/kanidm")"
        --auto-discover-url https://home.idm.meowcloud.net/oauth2/openid/forgejo/.well-known/openid-configuration
        --scopes openid --scopes email --scopes profile
      )
      id=$(forgejo admin auth list | ${getExe pkgs.gawk} '$2 == "kanidm" { print $1 }')
      if [ -n "$id" ]; then
        forgejo admin auth update-oauth --id "$id" "''${args[@]}"
      else
        forgejo admin auth add-oauth "''${args[@]}"
      fi
    '';
  };

  environment.systemPackages = [
    (pkgs.writeScriptBin "forgejo" ''
      #!${pkgs.runtimeShell}
      # sudo resets the environment, so pass it through env
      exec sudo -u forgejo ${getExe' pkgs.coreutils "env"} \
        FORGEJO_WORK_DIR=${config.services.forgejo.stateDir} \
        FORGEJO_CUSTOM=${config.services.forgejo.customDir} \
        ${getExe config.services.forgejo.package} "$@"
    '')
  ];
}
