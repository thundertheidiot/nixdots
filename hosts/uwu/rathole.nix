{config, ...}: {
  config = {
    sops.secrets."rathole" = {
      mode = "0644";
    };

    services.rathole = {
      enable = true;
      role = "client";
      # credentials from sops above
      credentialsFile = config.sops.secrets."rathole".path;
      settings = {
        client = {
          remote_addr = "gooptyland.xyz:2333";
          transport.type = "noise";

          services = {
            jellyfin.local_addr = "127.0.0.1:8096";
            bitwarden.local_addr = "127.0.0.1:8222";
            immich.local_addr = "localhost:2283";
          };
        };
      };
    };
  };
}
