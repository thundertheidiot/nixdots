{
  system = {
    config,
    pkgs,
    lib,
    ...
  }:
    lib.mkIf (config.workstation.enable) {
      environment.systemPackages = with pkgs; [
        udev-gothic-nf
        cantarell-fonts
        noto-fonts-color-emoji
        (catppuccin-gtk.override {
          accents = ["mauve"];
          size = "compact";
          variant = "mocha";
        })
        papirus-icon-theme
        catppuccin-cursors.mochaLavender
      ];

      # Catppuccin tty
      boot.kernelParams = [
        "vt.default_red=30,243,166,249,137,245,148,186,88,243,166,249,137,245,148,166"
        "vt.default_grn=30,139,227,226,180,194,226,194,91,139,227,226,180,194,226,173"
        "vt.default_blu=46,168,161,175,250,231,213,222,112,168,161,175,250,231,213,200"
      ];

      fonts.fontconfig = {
        enable = true;
        defaultFonts.serif = ["Cantarell"];
        defaultFonts.sansSerif = ["Cantarell"];
        defaultFonts.monospace = ["UDEV Gothic 35NF"];
        defaultFonts.emoji = ["Noto Color Emoji"];
      };
    };

  home = {
    config,
    pkgs,
    lib,
    ...
  }: {
    config = lib.mkIf (config.workstation.enable) (
      let
        qt-theme-package = pkgs.catppuccin-kde.override {
          accents = ["mauve"];
          flavour = ["mocha"];
        };
        gtk-theme-package = pkgs.catppuccin-gtk.override {
          accents = ["mauve"];
          size = "compact";
          variant = "mocha";
        };
      in (lib.mkMerge [
        {
          home.packages = with pkgs; [
            jetbrains-mono
            meslo-lgs-nf
            udev-gothic-nf
            cantarell-fonts
          ];

          fonts.fontconfig.enable = true;
        }
        (lib.mkIf (config.workstation.utils == "generic/gtk") (
          let
            cursor_package = pkgs.catppuccin-cursors.mochaLavender;
            cursor_name = "Catppuccin-Mocha-Lavender-Cursors";
          in {
            gtk = {
              enable = true;
              gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
              font = {
                package = pkgs.cantarell-fonts;
                name = "Cantarell";
                size = 12;
              };
              theme = {
                package = gtk-theme-package;
                name = "catppuccin-mocha-mauve-compact";
              };
              cursorTheme = {
                package = cursor_package;
                name = cursor_name;
                size = 24;
              };
              iconTheme = {
                package = pkgs.papirus-icon-theme;
                name = "Papirus-Dark";
              };
            };

            xdg.configFile = {
              "gtk-4.0/assets".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/assets";
              "gtk-4.0/gtk.css".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk.css";
              "gtk-4.0/gtk-dark.css".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk-dark.css";
            };

            qt = {
              enable = true;
              platformTheme.name = "qtct";
              style = {
                name = "kvantum";
              };
            };

            home.sessionVariables = {
              QT_QPA_PLATFORMTHEME = "qt5ct";
            };

            xdg.dataFile."Kvantum/Catppuccin-Mocha-Mauve" = {
              source = "${(pkgs.catppuccin-kvantum.override {
                accent = "Mauve";
                variant = "Mocha";
              })}/share/Kvantum/Catppuccin-Mocha-Mauve";
              recursive = true;
            };

            xdg.configFile."Kvantum/kvantum.kvconfig".text = ''
              [General]
              theme=Catppuccin-Mocha-Mauve
            '';

            home.pointerCursor = {
              package = cursor_package;
              name = cursor_name;
              size = 24;

              x11.defaultCursor = "left_ptr";
              x11.enable = true;
              gtk.enable = true;
            };
          }
        ))
        (lib.mkIf (builtins.elem "plasma" config.workstation.environment) {
          home.packages = with pkgs; [
            qt-theme-package
            papirus-icon-theme
          ];

          programs.plasma = let
            V = val: {
              value = val;
              immutable = false;
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
        })
      ])
    );
  };
}
