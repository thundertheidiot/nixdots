{
  pkgs,
  config,
  lib,
  ...
}: let
  inherit (lib) mkForce;
in {
  config = {
    catppuccin = {
      enable = true;
      flavor = "mocha";
      accent = "mauve";
    };

    # cursor
    home.pointerCursor = {
      enable = true;
      package = pkgs.adwaita-icon-theme;
      size = 24;
      name = "Adwaita";
      gtk.enable = true;
      x11.enable = true;
      hyprcursor.enable = true;
    };

    # gtk
    gtk = {
      enable = true;
      theme = {
        package = pkgs.catppuccin-gtk.override {
          accents = ["mauve"];
          size = "compact";
          variant = "mocha";
        };
        name = "catppuccin-mocha-mauve-compact";
      };
    };

    # qt
    qt = {
      enable = true;
      platformTheme.name = "qtct";
      style.name = "kvantum";
    };

    catppuccin.kvantum.enable = true;

    xdg.dataFile."Kvantum/catppuccin-mocha-mauve" = {
      source = "${(pkgs.catppuccin-kvantum.override {
        accent = "mauve";
        variant = "mocha";
      })}/share/Kvantum/catppuccin-mocha-mauve";
      recursive = true;
    };

    xdg.configFile = let
      s = source: mkForce {inherit source;};
    in {
      "Kvantum/kvantum.kvconfig".text = ''
        [General]
        theme=catppuccin-mocha-mauve
      '';
      "gtk-4.0/assets" = s "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/assets";
      "gtk-4.0/gtk.css" = s "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk.css";
      "gtk-4.0/gtk-dark.css" = s "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk-dark.css";
    };
  };
}
