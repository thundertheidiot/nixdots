{ ... }: {
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

      navidrome_env = {
        sopsFile = ./navidrome.env;
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
    };
  };
}
