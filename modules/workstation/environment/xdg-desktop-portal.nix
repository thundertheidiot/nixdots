{
  system = {config, pkgs, lib, ...}: lib.mkMerge [
    (lib.mkIf (builtins.elem "hyprland" config.workstation.environment) {
      xdg.portal.extraPortals = [
        pkgs.xdg-desktop-portal-hyprland
      ];

      xdg.portal.config = {
        hyprland.default = "hyprland";
      };
    })
    (lib.mkIf (builtins.elem "plasma" config.workstation.environment) {
      xdg.portal.extraPortals = [
        pkgs.kdePackages.xdg-desktop-portal-kde
      ];

      xdg.portal.configPackages = [pkgs.kdePackages.xdg-desktop-portal-kde];
    })
    (lib.mkIf (builtins.elem "cosmic" config.workstation.environment) {
      xdg.portal.extraPortals = [
        pkgs.xdg-desktop-portal-cosmic
      ];

      xdg.portal.config = {
        cosmic.default = "cosmic";
      };
    })
  ];

  home = {...}: {};
}
