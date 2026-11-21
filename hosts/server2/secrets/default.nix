{ ... }: {
  config = {
    sops.secrets = {
      homepage_env = {
        sopsFile = ./homepage.env;
        format = "dotenv";
      };

      navidrome_env = {
        sopsFile = ./navidrome.env;
        format = "dotenv";
      };

      bookorbit_env = {
        sopsFile = ./bookorbit.env;
        format = "dotenv";
      };

      soulbeet_env = {
        sopsFile = ./soulbeet.env;
        format = "dotenv";
      };

      home_assistant_secrets = {
        sopsFile = ./home-assistant.yaml;
        format = "yaml";
      };

      jellyfin_api_key = {
        sopsFile = ./jellyfin.yaml;
        format = "yaml";
        key = "api-key";
      };

      jellyfin_admin_password = {
        sopsFile = ./jellyfin.yaml;
        format = "yaml";
        key = "admin-password";
      };
    };
  };
}
