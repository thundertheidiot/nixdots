{
  config,
  pkgs,
  lib,
  mlib,
  ...
}: let
  inherit (mlib) mkEnOpt mkOpt;
  inherit (lib) mkIf;
  inherit (lib.types) enum;

  cfg = config.meow.workstation.theming;

  defaultFonts = {
    serif = ["Cantarell"];
    sansSerif = ["Cantarell"];
    monospace = ["UDEV Gothic 35NF"];
    emoji = ["Noto Color Emoji"];
  };
  fontPkgs = with pkgs; [
    udev-gothic-nf
    cantarell-fonts
    noto-fonts-color-emoji
  ];
in {
  options = {
    meow.workstation.theming = mkEnOpt "Theming";
    meow.workstation.theme = mkOpt (enum ["catppuccin_mocha"]) "catppuccin_mocha" {
      description = "Theme to use";
    };
  };

  imports = [
    ./catppuccin.nix
  ];

  config = lib.mkIf cfg {
    environment.systemPackages = fontPkgs;

    fonts.fontconfig = {
      enable = true;
      includeUserConf = true;
      inherit defaultFonts;
    };

    meow.home.modules = [
      {
        home.packages = fontPkgs;

        fonts.fontconfig = {
          enable = true;
          inherit defaultFonts;
        };
      }
      ({config, ...}: {
        gtk = {
          enable = true;
          gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
          font = {
            package = pkgs.cantarell-fonts;
            name = "Cantarell";
            size = 12;
          };
        };
      })
    ];
  };
}
