{ ... }: {
  config = {
    sops.secrets = {
      rathole_secrets = {
        sopsFile = ./rathole.toml;
        format = "binary";
      };
    };
  };
}
