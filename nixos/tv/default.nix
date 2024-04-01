{
  lib,
  config,
  pkgs,
  localconfig,
  ...
}: {
  config = lib.mkIf (config.setup.tv.enable) (with config; {
    services.xserver.displayManager.lightdm = {
      enable = true;
      autoLogin.timeout = 0;
    };

    services.xserver.displayManager.autoLogin = {
      enable = true;
      user = localconfig.username;
    };

    services.postgresql = {
      enable = false;
    };

    services.invidious = {
      enable = false;
      package = pkgs.invidious;
      address = "127.0.0.1";
      port = 3000;
      settings = {
        db = {
          user = "invidious";
          dbname = "invidious";
        };

        default_user_preferences = {
          disable_proxy = false;
          quality = "dash";
          quality_dash = "1080p";
        };
      };
    };
  });
}
