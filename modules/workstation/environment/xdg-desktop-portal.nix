{
  system = {config, pkgs, lib, ...}: lib.mkMerge [
    (lib.mkIf (builtins.elem "hyprland" config.workstation.environment) {
      xdg.portal.extraPortals = [
        pkgs.xdg-desktop-portal-hyprland
      ];

      xdg.portal.config = {
        hyprland = {
          default = "hyprland";
          "org.freedesktop.impl.portal.Secret" = "gnome-keyring";
        };
      };
    })
    (lib.mkIf (builtins.elem "plasma" config.workstation.environment) {
      xdg.portal.extraPortals = [
        pkgs.kdePackages.xdg-desktop-portal-kde
      ];

      xdg.portal.config = {
        kde = {
          default = "kde";
          "org.freedesktop.impl.portal.Secret" = "gnome-keyring";
        };
      };
    })
    (lib.mkIf (builtins.elem "cosmic" config.workstation.environment) {
      xdg.portal.extraPortals = [
        pkgs.xdg-desktop-portal-cosmic
      ];

      xdg.portal.config = {
        cosmic = {
          default = "cosmic";
          "org.freedesktop.impl.portal.Secret" = "gnome-keyring";
        };
      };
    })
  ];

  home = {...}: {};
}
