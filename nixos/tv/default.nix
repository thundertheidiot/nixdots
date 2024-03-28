{
  lib,
  config,
  pkgs,
  localconfig,
  ...
}: {
  config = lib.mkIf (localconfig.install.tv) (with config; {
    services.xserver.displayManager.lightdm = {
      enable = true;
      autoLogin.timeout = 0;
    };

    services.xserver.displayManager.autoLogin = {
      enable = true;
      user = localconfig.username;
    };

    services.postgresql = {
      enable = true;
    };

    services.invidious = {
      enable = true;
      address = "127.0.0.1";
      port = 3000;
      settings = {
        db = {
          user = "invidious";
          dbname = "invidious";
        };
      };
    };
  });
}
