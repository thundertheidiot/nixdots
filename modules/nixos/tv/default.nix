{
  lib,
  config,
  pkgs,
  inputs,
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

    services.displayManager.autoLogin = {
      enable = true;
      user = config.username;
    };

    systemd.services."ir-client" = let
      naersk = pkgs.callPackage inputs.naersk {};
      ir-client = naersk.buildPackage {
        src = ./ir-client;
      };
    in {
      enable = true;
      description = "Use tv remote as an input.";
      unitConfig = {
        Type = "simple";
      };
      serviceConfig = {
        ExecStart = "${ir-client}/bin/ir-client";
      };
      wantedBy = ["multi-user.target"];
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
