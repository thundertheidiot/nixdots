{...}: {
  sops.secrets."youtube_api_keys" = {
    sopsFile = ./youtube;
    format = "binary";
  };
}
