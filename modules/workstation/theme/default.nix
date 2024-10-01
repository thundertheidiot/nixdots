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
  inherit (lib.types) str;

  cfg = config.meow.workstation.theming;

  defaultFonts = {
    serif = ["Cantarell"];
    sansSerif = ["Cantarell"];
    monospace = ["UDEV Gothic 35NF"];
    emoji = ["Noto Color Emoji"];
  };
  # fontPkgs = with pkgs; [
  #   udev-gothic-nf
  #   cantarell-fonts
  #   noto-fonts-color-emoji
  # ];

  fontPkgs = [
    config.stylix.fonts.serif.package
    config.stylix.fonts.sansSerif.package
    config.stylix.fonts.monospace.package
    config.stylix.fonts.emoji.package
  ];
in {
  options = {
    meow.workstation.theming = mkEnOpt "Theming";
    meow.workstation.theme = mkOpt str "catppuccin-mocha" {
      description = "Theme to use";
    };
  };

  imports = [
    ./catppuccin.nix
  ];

  config = mkIf cfg (mkMerge [
    {
      environment.systemPackages = fontPkgs;
      fonts.fontconfig = {
        enable = true;
        includeUserConf = true;
        inherit defaultFonts;
      };

      boot = {
        plymouth.enable = true;

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

      meow.home.modules = [
        {
          home.packages = fontPkgs;

          stylix.targets = {
            emacs.enable = false;
            kde.enable = false;
            hyprpaper.enable = lib.mkForce false;
            waybar.enable = false;
          };

          fonts.fontconfig = {
            enable = true;
            # inherit defaultFonts;
          };
        }
        ({config, ...}: {
          gtk = {
            enable = true;
            gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
          };
        })
      ];

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
