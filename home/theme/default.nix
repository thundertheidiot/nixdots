{
  pkgs,
  mlib,
  ...
}:
let
  inherit (mlib) mkEnOptTrue;
in
{
  imports = [
    ./catppuccin.nix
    ./madoromi
  ];

  options.mHome.themeQt = mkEnOptTrue "qt";
  options.mHome.themeGtk = mkEnOptTrue "gtk";

  config = {
    home.pointerCursor = {
      enable = true;
      package = pkgs.adwaita-icon-theme;
      size = 24;
      name = "Adwaita";
      gtk.enable = true;
      x11.enable = true;
      hyprcursor.enable = true;
    };
  };
}
