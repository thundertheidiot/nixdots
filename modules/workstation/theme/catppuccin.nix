{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (lib) mkIf;

  cfg = config.meow.workstation.theming.enable;
  theme = config.meow.workstation.theme;
in {
  config = mkIf (cfg && theme == "catppuccin-mocha") {
    boot.kernelParams = [
      # mocha tty
      "vt.default_red=30,243,166,249,137,245,148,186,88,243,166,249,137,245,148,166"
      "vt.default_grn=30,139,227,226,180,194,226,194,91,139,227,226,180,194,226,173"
      "vt.default_blu=46,168,161,175,250,231,213,222,112,168,161,175,250,231,213,200"
    ];

    meow.home.modules = [
      ({config, ...}: let
        gtkPackage = pkgs.catppuccin-gtk.override {
          accents = ["mauve"];
          size = "compact";
          variant = "mocha";
        };
        gtkName = "catppuccin-mocha-mauve-compact";
        cursorPackage = pkgs.catppuccin-cursors.mochaLavender;
        cursorName = "Catppuccin-Mocha-Lavender-Cursors";
        iconPackage = pkgs.papirus-icon-theme;
        iconName = "Papirus-Dark";
      in {
        gtk = {
          theme = lib.mkForce {
            package = gtkPackage;
            name = gtkName;
          };
          iconTheme = lib.mkForce {
            package = iconPackage;
            name = iconName;
          };
        };

        qt = {
          enable = true;
          platformTheme.name = "qtct";
          style = {
            name = "kvantum";
          };
        };

        xdg.dataFile."Kvantum/Catppuccin-Mocha-Mauve" = {
          source = "${(pkgs.catppuccin-kvantum.override {
            accent = "Mauve";
            variant = "Mocha";
          })}/share/Kvantum/Catppuccin-Mocha-Mauve";
          recursive = true;
        };

        xdg.configFile = let
          s = source: lib.mkForce {inherit source;};
        in {
          "Kvantum/kvantum.kvconfig".text = ''
            [General]
            theme=Catppuccin-Mocha-Mauve
          '';
          "gtk-4.0/assets" = s "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/assets";
          "gtk-4.0/gtk.css" = s "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk.css";
          "gtk-4.0/gtk-dark.css" = s "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk-dark.css";
        };
      })
      (mkIf (builtins.elem "plasma" config.meow.workstation.environment) (let
        qtPackage = pkgs.catppuccin-kde.override {
          accents = ["mauve"];
          flavour = ["mocha"];
        };
      in {
        home.packages = with pkgs; [
          qtPackage
          papirus-icon-theme
        ];

        programs.plasma = let
          V = val: {
            value = val;
            immutable = true;
          };
        in {
          configFile = {
            "auroraerc"."CatppuccinMocha-Modern"."ButtonSize" = V 0;
            "kdeglobals"."KDE"."LookAndFeelPackage" = V "Catppuccin-Mocha-Mauve";
            "kdeglobals"."Icons"."Theme" = V "Papirus-Dark";
            "kdedefaults/kdeglobals"."Icons"."Theme" = V "Papirus-Dark";
            "kdedefaults/kdeglobals"."General"."ColorScheme" = V "CatppuccinMochaMauve";
          };
        };
      }))
    ];
  };
}
