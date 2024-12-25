{config, ...}: {
  config = {
    meow.sops.enableSecrets = ["server_vaultwarden_env"];
    meow.sops.secrets."server_vaultwarden.env" = {
      mode = "0644";
    };

    meow.impermanence.directories = [
      {
        path = "/var/lib/bitwarden_rs";
        persistPath = "${config.meow.impermanence.persist}/vaultwarden";
        user = toString config.users.users.vaultwarden.uid;
        group = toString config.users.groups.vaultwarden.gid;
      }
    ];

    users.users.vaultwarden.uid = 3000;
    users.groups.vaultwarden.gid = 3000;

    services.vaultwarden = {
      enable = true;
      dbBackend = "sqlite";
      backupDir = "/nix/persist/vaultwarden_backup";

      environmentFile = "/var/run/secrets/server_vaultwarden_env";

      config = {
        ROCKET_ADDRESS = "127.0.0.1";
        ROCKET_PORT = "8222";

        DOMAIN = "https://bw.kotiboksi.xyz";
        SIGNUPS_ALLOWED = false;
      };
    };
  };
}
