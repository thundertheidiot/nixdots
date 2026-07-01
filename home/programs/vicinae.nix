{config, ...}: let
  flavor = config.catppuccin.flavor;
in {
  services.vicinae = {
    systemd = {
      enable = true;
      autoStart = true;
      environment.USE_LAYER_SHELL = 1;
    };

    settings = {
      font.size = 12;
      window = {
        csd = true;
      };
      theme.dark = {
        name = "catppuccin-${flavor}";
        icon_theme = "default";
      };
    };
  };
}
