{
  pkgs,
  config,
  lib,
  theme,
  ...
}:
let
  inherit (lib) mkIf mkForce mkMerge;
  isCat = theme.family == "catppuccin";
  flavor = theme.variant;
  accent = "mauve";
  gtk = config.mHome.themeGtk;
  qt = config.mHome.themeQt;
in
{
  config = mkMerge [
    {
      catppuccin = {
        enable = isCat;
        autoEnable = isCat;
        flavor = mkIf isCat flavor;
        inherit accent;
      };
    }
    (mkIf (gtk && isCat) {
      gtk = {
        enable = true;
        theme = {
          package = pkgs.catppuccin-gtk.override {
            accents = [ accent ];
            size = "compact";
            variant = flavor;
          };
          name = "catppuccin-${flavor}-${accent}-compact";
        };
      };
      xdg.configFile =
        let
          s = source: mkForce { inherit source; };
        in
        {
          "gtk-4.0/assets" =
            s "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/assets";
          "gtk-4.0/gtk.css" =
            s "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk.css";
          "gtk-4.0/gtk-dark.css" =
            s "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk-dark.css";
        };
    })
    (mkIf (qt && isCat) {
      qt = {
        enable = true;
        platformTheme.name = "qtct";
        style.name = "kvantum";
      };
      catppuccin.kvantum.enable = true;
      xdg.dataFile."Kvantum/catppuccin-${flavor}-${accent}" = {
        source = "${
          pkgs.catppuccin-kvantum.override {
            inherit accent;
            variant = flavor;
          }
        }/share/Kvantum/catppuccin-${flavor}-${accent}";
        recursive = true;
      };
      xdg.configFile."Kvantum/kvantum.kvconfig".text = ''
        [General]
        theme=catppuccin-${flavor}-${accent}
      '';
    })
  ];
}
