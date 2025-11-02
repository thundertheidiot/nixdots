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

      forgejo_oauth_client_secret = {
        sopsFile = ./forgejo.yaml;
        format = "yaml";
        key = "oauth_client_secret";
      };

      authentik_env = {
        sopsFile = ./authentik.env;
        format = "dotenv";
      };
    };
  };
}
