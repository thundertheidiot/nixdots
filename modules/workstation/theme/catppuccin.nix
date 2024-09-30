{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (lib) mkIf;

  cfg = config.meow.workstation.theming;
  theme = config.meow.workstation.theme;
in {
  config = mkIf (cfg && theme == "catppuccin_mocha") {
    environment.variables = {
      QT_QPA_PLATFORMTHEME = "qt5ct";
    };

    boot = {
      plymouth = {
        enable = true;
        theme = "catppuccin-mocha";
        themePackages = [
          (pkgs.catppuccin-plymouth.override {
            variant = "mocha";
          })
        ];
      };

      consoleLogLevel = 0;
      initrd.verbose = false;
      kernelParams = [
        # mocha tty
        "vt.default_red=30,243,166,249,137,245,148,186,88,243,166,249,137,245,148,166"
        "vt.default_grn=30,139,227,226,180,194,226,194,91,139,227,226,180,194,226,173"
        "vt.default_blu=46,168,161,175,250,231,213,222,112,168,161,175,250,231,213,200"

        "quiet"
        "splash"
        "boot.shell_on_fail"
        "loglevel=3"
        "rd.systemd.show_status=false"
        "rd.udev.log_level=3"
        "udev.log_priority=3"
      ];
    };

    environment.systemPackages = [
      (pkgs.catppuccin-sddm.override {
        flavor = "mocha";
        fontSize = "12";
        font = "Cantarell";
      })
      ((pkgs.sddm-astronaut.override {
          themeConfig = {
            Font = "Cantarell";
            FontSize = "12";

            Background = "background.jpg";

            HighlightColor = "#f5e0dc";
            PlaceholderColor = "#313244";
            SystemButtonsIconColor = "#b4befe";
            BackgroundColor = "#1e1e2e";
            TextColor = "#cdd6f4";
          };
        })
        .overrideAttrs
        (prev: {
          installPhase =
            prev.installPhase
            + ''
              cp ${./background.jpg} $out/share/sddm/themes/sddm-astronaut-theme/background.jpg
            '';
        }))
    ];

    services.displayManager.sddm = {
      theme = "sddm-astronaut-theme";
      extraPackages = [
        pkgs.kdePackages.qt5compat
      ];
    };

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
        home.pointerCursor = {
          package = cursorPackage;
          name = cursorName;
          size = 24;

          x11.defaultCursor = "left_ptr";
          x11.enable = true;
          gtk.enable = true;
        };

        gtk = {
          theme = {
            package = gtkPackage;
            name = gtkName;
          };
          iconTheme = {
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

        xdg.configFile."Kvantum/kvantum.kvconfig".text = ''
          [General]
          theme=Catppuccin-Mocha-Mauve
        '';

        xdg.configFile = {
          "gtk-4.0/assets".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/assets";
          "gtk-4.0/gtk.css".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk.css";
          "gtk-4.0/gtk-dark.css".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk-dark.css";
        };
      })
      # TODO: move plasma away from old workstation
      (mkIf (builtins.elem "plasma" config.workstation.environment) (let
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
