{
  system = {
    lib,
    config,
    pkgs,
    ...
  }: lib.mkIf (config.workstation.environment == "plasma") {
    services.desktopManager.plasma6 = {
      enable = true;
      enableQt5Integration = true;
    };
  };

  home = {
    lib,
    config,
    pkgs,
    ...
  }: lib.mkIf (config.workstation.environment == "plasma") {
    
  };
}
