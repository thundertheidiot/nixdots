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

    networking.firewall.enable = false;
  });
}
