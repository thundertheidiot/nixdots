{
  shares = {
    directories = [
      "[VGM]/media/music"
      "[Metal]/media/metal"
      "[Breakcore]/media/breakcore"
    ];
  };
  transfers = {
    upload = {
      speed_limit = 1000;
    };
  };
  web = {
    authentication = {
      disabled = true;
      api_keys = {
        my_api_key = {
          role = "readonly";
          cidr = "0.0.0.0/0,::/0";
        };
        soulbeet = {
          role = "readonly";
          cidr = "0.0.0.0/0, ::/0";
        };
      };
    };
    port = 5030;
  };
  directories = {
    downloads = "/app/downloads";
    incomplete = "/app/incomplete";
  };
  remote_configuration = false;
}
