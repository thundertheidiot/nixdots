{...}: {
  config = {
    sops.secrets = {
      coturn_secret = {
        sopsFile = ./coturn-secret;
        owner = "turnserver";
        group = "turnserver";
        mode = "0440";
        format = "binary";
      };

      sodexobot_env = {
        sopsFile = ./sodexobot.env;
        format = "dotenv";
      };

      authentik_env = {
        sopsFile = ./authentik.env;
        format = "dotenv";
      };

      vaultwarden_env = {
        sopsFile = ./vaultwarden.env;
        format = "dotenv";
      };

      meowdzbot_env = {
        sopsFile = ./meowdzbot.env;
        format = "dotenv";
      };
    };
  };
}
