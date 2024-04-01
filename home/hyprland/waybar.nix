{
  config,
  lib,
  ...
}: let
  colors = with config.scheme.withHashtag; {
    background = base00;
    foreground = base07;
    inherit base00 base01 base02 base03 base04 base05 base06 base07 base08 base09 base10 base11 base12 base13 base14 base15;
  };
in {
  config = lib.mkIf (config.setup.hyprland.enable) (with config; {
    programs.waybar = {
      enable = true;
      style = ''
        * {
            all: unset;
            border: none;
            border-radius: 4px;
            font-family: Cantarell;
            font-size: 12px;
            min-height: 0;
        }

        window#waybar {
            background: @theme_base_color;
            background-color: ${colors.background};
            border-bottom: 3px solid ${colors.base08};
            color: @theme_text_color;
            transition-property: background-color;
            transition-duration: .5s;
            border-radius: 0;
        }

        window#waybar.hidden {
            opacity: 0.2;
        }

        tooltip {
          background: ${colors.background};
          border: 1px solid ${colors.foreground};
        }

        tooltip label {
          color: @theme_text_color;
        }

        /*
        window#waybar.empty {
            background-color: transparent;
        }
        window#waybar.solo {
            background-color: #FFFFFF;
        }
        */

        #workspaces button {
            padding: 0 0.7em;
            background-color: transparent;
            color: ${colors.foreground};
            box-shadow: inset 0 -3px transparent;
        }

        #workspaces button:hover {
            background: rgba(0, 0, 0, 0.2);
            box-shadow: inset 0 -3px ${colors.foreground};
        }

        #workspaces button.urgent {
            background-color: ${colors.base01};
        }

        #clock,
        #battery,
        #cpu,
        #memory,
        #disk,
        #temperature,
        #backlight,
        #network,
        #pulseaudio,
        #custom-weather,
        #tray,
        #mode,
        #idle_inhibitor,
        #custom-notification,
        #sway-scratchpad,
        #mpd {
            padding: 0 10px;
            margin: 6px 3px;
            background-color: ${colors.foreground};
            color: ${colors.background};
        }

        #window,
        #workspaces {
            margin: 0 4px;
        }

        /* If workspaces is the leftmost module, omit left margin */
        .modules-left > widget:first-child > #workspaces {
            margin-left: 0;
        }

        /* If workspaces is the rightmost module, omit right margin */
        .modules-right > widget:last-child > #workspaces {
            margin-right: 0;
        }

        #clock {
            background-color: ${colors.foreground};
            color: ${colors.background};
        }

        #battery {
            background-color: ${colors.foreground};
            color: ${colors.background};
        }

        #battery.charging, #battery.plugged {
            color: ${colors.background};
            background-color: ${colors.base15};
        }

        @keyframes blink {
            to {
                background-color: #ffffff;
                color: #000000;
            }
        }

        #battery.critical:not(.charging) {
            background-color: ${colors.base08};
            color: ${colors.background};
            animation-name: blink;
            animation-duration: 0.5s;
            animation-timing-function: linear;
            animation-iteration-count: infinite;
            animation-direction: alternate;
        }

        label:focus {
            background-color: #000000;
        }

        #cpu {
            background-color: ${colors.base03};
            color: ${colors.background};
        }

        #memory {
            background-color: ${colors.base03};
            color: ${colors.background};
        }

        #backlight {
            background-color: ${colors.base03};
        }

        #network {
            background-color: ${colors.foreground};
            color: ${colors.background};
        }

        #network.disconnected {
            background-color: ${colors.base08};
            color: ${colors.background};
        }

        #pulseaudio {
            background-color: ${colors.foreground};
            color: ${colors.background};
        }

        #pulseaudio.muted {
            background-color: ${colors.base08};
            color: ${colors.background};
        }

        #temperature {
            background-color: ${colors.base12};
            color: ${colors.background};
        }

        #temperature.critical {
            background-color: ${colors.base04};
            color: ${colors.background};
        }

        #tray {
            background-color: ${colors.base05}
        }

        #tray > .passive {
            -gtk-icon-effect: dim;
        }

        #tray > .needs-attention {
            -gtk-icon-effect: highlight;
            background-color: ${colors.base04};
        }

        #idle_inhibitor {
            background-color: ${colors.foreground};
            color: ${colors.background};
        }

        #idle_inhibitor.activated {
            background-color: ${colors.base05};
            color: ${colors.background};
        }

        #mpd {
            background-color: ${colors.base02};
            color: ${colors.background};
        }

        #mpd.disconnected {
            background-color: ${colors.base04};
        }

        #mpd.stopped {
            background-color: ${colors.base00};
        }

        #mpd.paused {
            background-color: ${colors.base08};
        }

        #language {
            background-color: ${colors.foreground};
            color: ${colors.background};
            padding: 0 5px;
            margin: 6px 3px;
            min-width: 16px;
        }

        #keyboard-state {
            background-color: ${colors.base00};
            color: ${colors.background};
            padding: 0 0px;
            margin: 0 5px;
            min-width: 16px;
        }

        #keyboard-state > label {
            padding: 0 5px;
        }

        #keyboard-state > label.locked {
            background: rgba(0, 0, 0, 0.2);
        }

        #disk {
            background-color: ${colors.foreground};
            color: ${colors.background};
        }
      '';
      settings = [
        {
          layer = "top";
          position = "top";
          height = 30;
          modules-left = ["hyprland/workspaces" "hyprland/window"];
          modules-center = ["clock"];
          modules-right = ["hyprland/language" "idle_inhibitor" "disk#root" "network" "pulseaudio" "battery" "tray"];

          "hyprland/workspaces" = {
            disable-scroll = true;
            active-only = false;
          };

          "hyprland/window" = {
            format = "{title}";
            max-length = 20;
            separate-outputs = true;
          };

          "clock" = {
            interval = 1;
            timezone = "${config.timeZone}";
            format = "{:%d.%m.%Y %T}";
          };

          "hyprland/language" = {
            format = "{short}";
            tooltip-format = "{long}";
          };

          "idle_inhibitor" = {
            format = "{icon}";
            format-icons = {
              activated = "";
              deactivated = "";
            };
          };

          "disk#root" = {
            interval = 30;
            format = "{path} {free}";
            path = "/";
            warning = 80;
            critical = 90;
          };

          "network" = {
            format-wifi = "{essid} ({signalStrength}%) ";
            format-ethernet = "{ifname}: {ipaddr}/{cidr}";
            format-linked = "No Internet ⚠";
            format-disconnected = "Disconnected ⚠";
          };

          "pulseaudio" = {
            scroll-step = 3;
            format = "{volume}% {icon}";
            format-bluetooth = "{volume}% {icon}";
            format-icons = {
              default = ["" "" ""];
            };
            escape = true;
          };

          "battery" = {
            states = {
              warning = 30;
              critical = 15;
            };
            format = "{capacity}% {icon} ({time})";
            format-charging = "{capacity}%  ({time})";
            format-plugged = "{capacity}%  ({time})";
            format-icons = ["" "" "" "" ""];
          };

          "tray" = {
            spacing = 10;
          };
        }
      ];
    };
  });
}
