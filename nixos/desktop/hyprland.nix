{
  lib,
  config,
  pkgs,
  localconfig,
  inputs,
  ...
}:
lib.mkIf (localconfig.install.hyprland) (with config; {
  # Hyprland is configured by home-manager
  programs.hyprland = {
    enable = true;
    package = inputs.hyprland.packages.${pkgs.system}.hyprland;
    portalPackage = inputs.hyprland.packages.${pkgs.system}.xdg-desktop-portal-hyprland;
  };
})
