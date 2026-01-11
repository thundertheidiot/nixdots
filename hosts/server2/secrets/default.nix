{...}: {
  config = {
    sops.secrets = {
      torrent_stack_env = {
        sopsFile = ./torrent_stack_env;
        format = "binary";
      };

      homepage_env = {
        sopsFile = ./homepage.env;
        format = "dotenv";
      };

      home_assistant_secrets = {
        sopsFile = ./home-assistant.yaml;
        format = "yaml";
      };

      urlwatch_urls = {
        sopsFile = ./urlwatch_urls.yaml;
        format = "yaml";
        key = "";
      };

      urlwatch_config = {
        sopsFile = ./urlwatch_config.yaml;
        format = "yaml";
        key = "";
      };
    };
  };
}
