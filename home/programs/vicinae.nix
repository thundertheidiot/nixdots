{...}: {
  services.vicinae = {
    systemd.enable = true;

    settings = {
      font.size = 12;
      window = {
        csd = true;
      };
    };
  };
}
