{
  mlib,
  lib,
  config,
  ...
}: let
  inherit (mlib) homeModule mkOpt;
  inherit (lib) mkIf;
  inherit (lib.types) listOf str;
  inherit (lib.lists) unique elem;
  inherit (lib.attrsets) mapAttrs' mapAttrsToList filterAttrs;
  inherit (builtins) replaceStrings;

  work = config.meow.workstation.enable;
  env = config.meow.workstation.environment;
in {
  options = {
    meow.workstation.waybarDiskFilter =
      mkOpt (listOf str)
      []
      {
        description = "Mountpoints to filter out in waybar.";
      };
  };

  config = mkIf (work && elem "hyprland" env) (homeModule {
    programs.waybar.settings = let
      diskName = name:
        if name == null
        then "NULL"
        else replaceStrings ["/"] ["_"] name;

      filterList =
        ["/boot"] ++ lib.lists.optional config.meow.impermanence.enable "/" ++ config.meow.workstation.waybarDiskFilter;

      fileSystems =
        filterAttrs (_: d: !elem d.mountPoint filterList)
        config.fileSystems;

      disks =
        mapAttrs' (n: v: {
          name = "disk#${diskName v.device}";
          value = {
            interval = 30;
            format = "{path} {free}";
            path = n;
            warning = 80;
            critical = 90;
          };
        })
        fileSystems;
    in [
      ({
          layer = "top";
          position = "top";
          height = 32;
          modules-left = ["hyprland/workspaces" "hyprland/window"];
          modules-center = ["clock"];
          modules-right =
            ["hyprland/language" "idle_inhibitor"]
            ++ unique (mapAttrsToList (_: fs: "disk#${replaceStrings ["/"] ["_"] fs.device}")
              fileSystems)
            ++ ["network" "pulseaudio" "battery" "tray"];

          "hyprland/workspaces" = {
            format = "{icon}";
            format-icons = {
              urgent = "";
              active = "";
              visible = "";
              default = "";
              empty = "";
            };
            disable-scroll = true;
            active-only = false;
            all-outputs = false;
            persistent-workspaces = {
              "*" = 9;
            };
          };

          "hyprland/window" = {
            format = "{title}";
            max-length = 40;
            separate-outputs = true;
            empty-format = "Desktop";
          };

          "clock" = {
            interval = 1;
            timezone = "${config.time.timeZone}";
            format = "{:%a, %d %b %H:%M}";
          };

          "hyprland/language" = {
            format = " {short}";
            tooltip-format = "{long}";
          };

          "idle_inhibitor" = {
            format = "{icon} ";
            format-icons = {
              activated = "";
              deactivated = "";
            };
          };

          "network" = {
            format-wifi = " {essid} ({signalStrength}%)";
            format-ethernet = "󰈀 {ifname}: {ipaddr}/{cidr}";
            format-linked = "No Internet ⚠";
            format-disconnected = "  Disconnected";
          };

          "pulseaudio" = {
            scroll-step = 3;
            format = "{volume}% {icon}";
            format-muted = " Muted";

            format-bluetooth = "{volume}% {icon} ";
            format-bluetooth-muted = " Muted  ";

            format-icons = {
              default = ["" "" ""];
            };
            escape = true;
            on-click = "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
          };

          "battery" = {
            states = {
              warning = 30;
              critical = 15;
            };
            format = "{capacity}% {icon} ({time})";
            format-charging = "{capacity}%  ({time})";
            format-plugged = "{capacity}%  ({time})";
            format-alt = "{capacity}% {icon} ({time})";
            format-icons = ["" "" "" "" ""];
          };

          "tray" = {
            spacing = 10;
          };
        }
        // disks)
    ];

    programs.waybar.style = let
      colors = config.lib.stylix.colors.withHashtag;
      c = config.lib.stylix.colors;
    in ''
      * {
          border: none;
          font-family: sans-serif, "Symbols Nerd Font";
          font-size: 14px;
          min-height: 0;
        }

        window#waybar {
          background: rgba(${c.base01-rgb-r}, ${c.base01-rgb-g}, ${c.base01-rgb-b}, 0.75);
          /*color: ${colors.base05};*/
          margin: 4px 6px;
          /*border-radius: 10px; */
          /*border: 1px solid ${colors.base03};*/
        }

        /* General modules */
        #workspaces,
        #idle_inhibitor,
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
        #custom-notification,
        #sway-scratchpad,
        #window,
        #mpd {
          background: ${colors.base01};
          color: ${colors.base05};
          padding: 6px 12px;
          margin: 4px 4px;
          border-radius: 10px;
          border: 1px solid ${colors.base03};
          box-shadow: 0 2px 6px rgba(0,0,0,0.25);
          transition: background 0.3s ease, color 0.3s ease;
        }

        /* Hover effect */
        #workspaces button:hover,
        #idle_inhibitor:hover,
        #clock:hover,
        #battery:hover,
        #network:hover,
        #pulseaudio:hover {
          background: ${colors.base04};
          color: ${colors.base00};
        }

        /* Workspaces styling */
        #workspaces {
          padding: 4px;
        }

        #workspaces button {
          padding: 4px 8px;
          margin: 2px;
          border-radius: 8px;
          background: ${colors.base02};
          color: ${colors.base05};
          border: none;
          transition: background 0.2s ease;
        }

        #workspaces button.active {
          background: ${colors.base08};
          color: ${colors.base00};
          font-weight: bold;
        }

        #workspaces button.empty {
          background: transparent;
          color: ${colors.base04};
        }

        /* Keyboard state (caps lock, etc.) */
        #language {
          background: ${colors.base02};
          color: ${colors.base05};
          padding: 0 8px;
          margin: 4px;
          border-radius: 8px;
          border: 1px solid ${colors.base03};
        }
    '';
  });
}
