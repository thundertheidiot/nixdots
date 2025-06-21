{
  config,
  pkgs,
  lib,
  mlib,
  inputs,
  ...
}: let
  inherit (mlib) mkEnOpt mkOpt;
  inherit (lib) mkIf mkMerge;
  inherit (lib.types) str anything;

  cfg = config.meow.workstation.theming;

  defaultFonts = {
    serif = ["Cantarell"];
    sansSerif = ["Cantarell"];
    monospace = ["UDEV Gothic 35NF"];
    emoji = ["Noto Color Emoji"];
  };

  fontPkgs = [
    config.stylix.fonts.serif.package
    config.stylix.fonts.sansSerif.package
    config.stylix.fonts.monospace.package
    config.stylix.fonts.emoji.package
  ];
in {
  options = {
    meow.workstation.theming.enable = mkEnOpt "Theming";
    meow.workstation.theme = mkOpt str "catppuccin-mocha" {
      description = "Theme to use";
    };
    meow.workstation.theming.iconTheme = {
      package = mkOpt anything pkgs.papirus-icon-theme {
        description = "Icon theme package";
      };
      name = mkOpt str "Papirus-Dark" {};
    };
  };

  imports = [
    ./catppuccin.nix
    ./misc.nix
  ];

  config = mkIf cfg.enable (mkMerge [
    {
      fonts.fontconfig = {
        enable = true;
        includeUserConf = true;
      };

      stylix.targets.plymouth.enable = false;

      boot = {
        plymouth = {
          enable = true;

          themePackages = with pkgs; [
            plymouth-blahaj-theme
          ];

          theme = "blahaj";
        };

        loader.timeout = 0;

        consoleLogLevel = 0;
        initrd.verbose = false;

        kernelParams = [
          "quiet"
          "splash"
          "boot.shell_on_fail"
          "loglevel=3"
          "rd.systemd.show_status=false"
          "rd.udev.log_level=3"
          "udev.log_priority=3"
        ];
      };

      environment.systemPackages =
        fontPkgs
        ++ [
          # SDDM theme
          ((pkgs.sddm-astronaut.override {
              themeConfig = let
                colors = config.lib.stylix.colors.withHashtag;
              in {
                Font = config.stylix.fonts.serif.name;
                FontSize = "12";

                Background = "background.jpg";

                HighlightColor = colors.base05;
                PlaceholderColor = colors.base04;
                SystemButtonsIconColor = colors.base04;
                BackgroundColor = colors.base00;
                TextColor = colors.base01;
              };
            })
            .overrideAttrs
            (prev: {
              installPhase =
                prev.installPhase
                # TODO: make a system
                + ''
                  cp ${./background.jpg} $out/share/sddm/themes/sddm-astronaut-theme/background.jpg
                '';
            }))
        ];

      services.displayManager.sddm = {
        theme = "sddm-astronaut-theme";
        extraPackages = [
          pkgs.kdePackages.qt5compat
          pkgs.kdePackages.qtmultimedia
          pkgs.libsForQt5.phonon
        ];
      };

      meow.home.modules = [
        {
          home.packages = fontPkgs ++ [cfg.iconTheme.package];

          stylix.targets = {
            emacs.enable = false;
            kde.enable = false;
            hyprpaper.enable = lib.mkForce false;
            waybar.enable = false;
          };

          fonts.fontconfig = {
            enable = true;
          };
        }
        ({config, ...}: {
          gtk = {
            enable = true;
            gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
          };

          xdg.configFile."qt5ct/qt5ct.conf".text = lib.generators.toINI {} {
            Appearance = {
              standard_dialogs = "default";
              style = "kvantum";
              icon_theme = cfg.iconTheme.name;
            };
            Fonts = {
              #        name,   size, ?,?, ?,?,?,?,?,?
              fixed = "Monospace,10,-1,5,50,0,0,0,0,0";
              general = "Sans Serif,12,-1,5,50,0,0,0,0,0";
            };
            Interface = {
              activate_item_on_single_click = 0;
            };
          };

          # TODO: qt6 titlebars broken, test
          xdg.configFile."qt6ct/qt6ct.conf".text = lib.generators.toINI {} {
            Appearance = {
              standard_dialogs = "default";
              style = "kvantum";
              icon_theme = cfg.iconTheme.name;
            };
            Fonts = {
              #        name,   size, ?,?, ?,?,?,?,?,?
              fixed = "Monospace,10,-1,5,400,0,0,0,0,0,0,0,0,0,0,1";
              general = "Sans Serif,12,-1,5,400,0,0,0,0,0,0,0,0,0,0,1";
            };
            Interface = {
              activate_item_on_single_click = 0;
            };
          };
        })
      ];

      stylix.targets = {
        fish.enable = false;
        # qt.platform = "kde6";
      };

      stylix.enable = true;
      stylix.autoEnable = true;

      stylix.base16Scheme = "${inputs.tt-schemes}/base16/${config.meow.workstation.theme}.yaml";
      stylix.image = ./background.jpg;

      stylix.cursor = {
        package = pkgs.adwaita-icon-theme;
        name = "Adwaita";
        size = 24;
      };

      stylix.fonts = {
        serif = {
          package = pkgs.cantarell-fonts;
          name = "Cantarell";
        };
        sansSerif = {
          package = pkgs.cantarell-fonts;
          name = "Cantarell";
        };
        monospace = {
          package = pkgs.udev-gothic-nf;
          name = "UDEV Gothic 35NF";
        };
        emoji = {
          package = pkgs.noto-fonts-color-emoji;
          name = "Noto Color Emoji";
        };

        sizes = {
          terminal = 11;
        };
      };
    }
  ]);
}
