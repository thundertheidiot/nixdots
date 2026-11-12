{
  pkgs,
  config,
  mlib,
  lib,
  ...
}:
let
  inherit (lib) mkIf getExe';
  inherit (mlib) mkEnOptTrue;

  work = config.meow.workstation.enable;
  cfg = config.meow.workstation.japaneseInput.enable;
in
{
  options = {
    meow.workstation.japaneseInput.enable = mkEnOptTrue "Japanese input through fcitx5 and mozc";
  };

  config = mkIf (work && cfg) {
    i18n.inputMethod = {
      enable = true;
      type = "fcitx5";
      fcitx5 = {
        waylandFrontend = true;
        addons = [ pkgs.fcitx5-mozc ];
        settings.globalOptions."Hotkey/TriggerKeys"."0" = "Alt+space";
        settings.inputMethod = {
          GroupOrder."0" = "Default";
          "Groups/0" = {
            Name = "Default";
            "Default Layout" = "us";
            DefaultIM = "mozc";
          };
          "Groups/0/Items/0".Name = "keyboard-us";
          "Groups/0/Items/1".Name = "mozc";
        };
      };
    };

    meow.workstation.extraNiriConfig = [
      ''
        spawn-at-startup "${getExe' config.i18n.inputMethod.package "fcitx5"}"
      ''
    ];

    # the xdg autostart unit races niri and starts fcitx5 without WAYLAND_DISPLAY, niri spawns it instead
    meow.home.modules = [
      {
        xdg.configFile."autostart/org.fcitx.Fcitx5.desktop".text = ''
          [Desktop Entry]
          Hidden=true
        '';
      }
    ];

    fonts = {
      packages = with pkgs; [
        noto-fonts-cjk-sans
        noto-fonts-cjk-serif
      ];

      # prefer the jp glyph variants over chinese ones
      fontconfig.defaultFonts = {
        sansSerif = [ "Noto Sans CJK JP" ];
        serif = [ "Noto Serif CJK JP" ];
      };
    };
  };
}
