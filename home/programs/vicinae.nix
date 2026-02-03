{...}: {
  services.vicinae = {
    systemd.enable = true;

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
