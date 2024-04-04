{
  lib,
  config,
  pkgs,
  inputs,
  ...
}:
lib.mkIf (config.setup.hyprland.enable) (with config; {
  programs.hyprland = {
    enable = true;
    package = inputs.hyprland.packages.${pkgs.system}.hyprland;
    portalPackage = inputs.hyprland.packages.${pkgs.system}.xdg-desktop-portal-hyprland;
  };
})
