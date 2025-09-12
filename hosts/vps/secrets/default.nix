{...}: {
  config = {
    sops.secrets = {
      coturn_secret = {
        sopsFile = ./coturn-secret;
        format = "binary";
      };
    };
  };
}
