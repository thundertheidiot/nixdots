{...}: {
  sops.secrets."youtube_api_keys" = {
    sopsFile = ./youtube;
    format = "binary";
  };

  sops.secrets."rathole" = {
    sopsFile = ./rathole.toml;
    format = "binary";
  };
}
