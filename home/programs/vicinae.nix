{...}: {
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
        name = "catppuccin-mocha";
        icon_theme = "default";
      };
    };
  };
}
