{ config, ... }: {
  config = {
    networking.firewall.allowedTCPPorts = [
      2333
      25565
    ];

    services.rathole = {
      enable = true;
      role = "server";
      credentialsFile = config.sops.secrets."rathole_secrets".path;
      settings = {
        server = {
          bind_addr = "0.0.0.0:2333";
          transport.type = "noise";

          services.minecraft.bind_addr = "0.0.0.0:25565";
        };
      };
    };
  };
}
