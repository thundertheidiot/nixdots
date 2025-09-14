{...}: {
  config = {
    sops.secrets = {
      coturn_secret = {
        sopsFile = ./coturn-secret;
        format = "binary";
      };

      sodexobot_env = {
        sopsFile = ./sodexobot.env;
        format = "dotenv";
      };
    };
  };
}
