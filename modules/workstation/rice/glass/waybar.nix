{
  config,
  pkgs,
  lib,
  ...
}:
let
  inherit (pkgs) replaceVars;
  inherit (lib) mkIf;

  cfg = config.meow.rice;
in
{
  config = mkIf (cfg == "glass") {
    meow.workstation.waybarDiskFormat = " {path} {free}";

    meow.home.modules = [
      {
        catppuccin.waybar.enable = false;
        programs.waybar.style =
          let
            colors = config.meow.workstation.theme.palette.withHashtag;
          in
          replaceVars ./waybar.css {
            accent = colors.base0D;

            fg = colors.base05;
            border = colors.base02;
            borderHover = colors.base03;
            muted = colors.base04;
            warn = colors.base0A;
            danger = colors.base08;

            inherit (colors) base00 base01 base02;
          };

        programs.waybar.settings.main = {
          layer = "top";
          position = "top";
          height = 36;
          spacing = 8;

          "idle_inhibitor" = {
            format = "{icon}";
            format-icons = {
              activated = "";
              deactivated = "";
            };
          };

          network = {
            format-wifi = " {essid} ({signalStrength}%)";
            format-ethernet = "󰈀 {ifname}: {ipaddr}/{cidr}";
            format-linked = "No Internet ⚠";
            format-disconnected = "  Disconnected";
            tooltip-format = "{ifname}  {ipaddr}/{cidr}\n{gwaddr}";
          };

          "custom/swaync" = {
            format = "{icon}";
            "format-icons" = {
              notification = "<span foreground='red'><sup></sup></span>";
              none = "";
              "dnd-notification" = "<span foreground='red'><sup></sup></span>";
              "dnd-none" = "";
              "inhibited-notification" = "<span foreground='red'><sup></sup></span>";
              "inhibited-none" = "";
              "dnd-inhibited-notification" = "<span foreground='red'><sup></sup></span>";
              "dnd-inhibited-none" = "";
            };
          };

          pulseaudio = {
            format = "{icon} {volume}%";
            format-muted = "󰝟 Muted";
            format-bluetooth = "{icon} {volume}% ";
            format-bluetooth-muted = " Muted ";
            format-icons = {
              default = [
                ""
                ""
                ""
              ];
            };
          };

          power-profiles-daemon = {
            format = " {icon} ";
            tooltip-format = "Profile: {profile}\nDriver: {driver}";
            tooltip = true;
            format-icons = {
              default = "󰚥";
              performance = "󰚥";
              balanced = "";
              power-saver = "";
            };
          };

          battery = {
            format = "{icon} {capacity}% ({time})";
            format-charging = " {capacity}% ({time})";
            format-plugged = " {capacity}%";
            format-icons = [
              ""
              ""
              ""
              ""
              ""
            ];
          };
        };
      }
    ];
  };
}
