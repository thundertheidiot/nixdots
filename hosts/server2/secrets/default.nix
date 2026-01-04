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
    };
  };
}
