{ pkgs, config, lib, ... }: {
  config = lib.mkIf (config.setup.swayfx.enable) ({
    wayland.windowManager.sway = {
      enable = true;
      package = pkgs.swayfx;

      xwayland = true;
      systemd.enable = true;
      wrapperFeatures = {
        base = true;
        gtk = true;
      };

      config = {

      };
    };
  });
}
