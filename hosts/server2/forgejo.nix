{
  config,
  lib,
  pkgs,
  ...
}:
let
  certs = import ../../certs;
in
{
  server.domains = [ "git.home" ];

  services.nginx.virtualHosts."git.home" = {
    forceSSL = true;
    sslCertificate = certs."local.crt";
    sslCertificateKey = config.sops.secrets.localKey.path;
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
      ROOT_URL = "https://git.home/";
      HTTP_PORT = 3001;
    };
  };

  environment.systemPackages = [
    (pkgs.writeScriptBin "forgejo" ''
      #!${pkgs.runtimeShell}
      export GITEA_WORK_DIR=${config.services.forgejo.stateDir}
      export GITEA_CUSTOM=${config.services.forgejo.customDir}
      exec sudo -u forgejo ${lib.getExe config.services.forgejo.package} "$@"
    '')
  ];
}
