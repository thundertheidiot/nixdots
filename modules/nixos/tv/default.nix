{
  lib,
  config,
  pkgs,
  ...
}: {
  config = lib.mkIf (config.setup.tv.enable) (with config; {
    services.displayManager.sddm = {
      settings = {
        Autologin = {
          Session = "hyprland.desktop";
          User = "${config.username}";
        };
      };
    };

    services.xserver.displayManager.autoLogin = {
      enable = true;
      user = config.username;
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
