{...}: {
  config = {
    meow.sops.enableSecrets = ["server_rathole"];
    meow.sops.secrets."server_rathole" = {
      mode = "0644";
    };

    services.rathole = {
      enable = true;
      role = "client";
      # credentials from sops above
      credentialsFile = "/var/run/secrets/server_rathole";
      settings = {
        client = {
          remote_addr = "gooptyland.xyz:2333";
          transport.type = "noise";

          services = {
            jellyfin.local_addr = "127.0.0.1:8096";
          };

          services = {
            bitwarden.local_addr = "127.0.0.1:8222";
          };
        };
      };
    };
  };
}
