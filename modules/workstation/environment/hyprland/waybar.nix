{
  mlib,
  lib,
  config,
  ...
}: let
  inherit (mlib) homeModule mkOpt;
  inherit (lib.types) listOf str;
  inherit (lib.lists) unique elem;
  inherit (lib.attrsets) mapAttrs' mapAttrsToList filterAttrs;
  inherit (builtins) replaceStrings;
in {
  options = {
    meow.workstation.waybarDiskFilter =
      mkOpt (listOf str)
      []
      {
        description = "Mountpoints to filter out in waybar.";
      };
  };

  config = homeModule {
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
          height = 30;
          modules-left = ["hyprland/workspaces" "hyprland/window"];
          modules-center = ["clock"];
          modules-right =
            ["hyprland/language" "idle_inhibitor"]
            ++ unique (mapAttrsToList (_: fs: "disk#${replaceStrings ["/"] ["_"] fs.device}")
              fileSystems)
            ++ ["network" "pulseaudio" "battery" "tray"];

          "hyprland/workspaces" = {
            format = "{name}";
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
            timezone = "${config.time.timeZone}";
            format = "{:%d.%m.%Y (%a) %T}";
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
        // disks)
    ];

    # programs.waybar.style = let
    #   colors = config.lib.stylix.colors.withHashtag;
    # in ''
    #   * {
    #     border: none;
    #     font-family: sans;
    #     font-size: 12px;

    #     border-radius: 0px;
    #   }

    #   #workspaces button:hover {
    #     background-color: ${colors.base05};
    #     color: ${colors.base00};
    #   }
    # '';
  };
}
