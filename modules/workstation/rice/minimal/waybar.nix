{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (pkgs) replaceVars;
  inherit (lib) mkIf mkForce;

  cfg = config.meow.rice;
in {
  config = mkIf (cfg == "minimal") {
    meow.workstation.waybarDiskFormat = "{path} {free}";

    meow.home.modules = [
      {
        catppuccin.waybar.enable = false;
        programs.waybar.style = let
          colors = config.meow.workstation.theme.palette.withHashtag;
        in
          replaceVars ./waybar.css {
            accent = colors.base0D;
            bg = colors.base00;
            fg = colors.base05;
            inactive = colors.base03;
          };

        programs.waybar.settings.main = {
          layer = "top";
          position = "top";
          height = 24;
          spacing = 0;

          modules-left = mkForce ["niri/window"];
          modules-center = mkForce [];
          modules-right = mkForce [
            "network"
            "pulseaudio"
            "battery"
            "tray"
            "clock"
          ];

          "niri/window" = {
            format = " {title}";
            # DWM title expands, so we can remove max-length or set it very high
            max-length = 200;
          };

          clock = {
            # Separator baked into the format
            format = mkForce " | {:%a %d %b %H:%M}";
          };

          network = {
            format-wifi = "W: {signalStrength}%";
            format-ethernet = "E: {ipaddr}";
            format-disconnected = "No Net";
          };

          pulseaudio = {
            # next to tray
            format = " | V: {volume}% | ";
            format-muted = " | V: Mute | ";
          };

          battery = {
            format = " | B: {capacity}%";
            format-charging = " | C: {capacity}%";
            format-plugged = " | P: {capacity}%";
            states = {
              warning = 30;
              critical = 15;
            };
          };

          tray = {
            spacing = mkForce 4;
          };
        };
      }
    ];
  };
}
