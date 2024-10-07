{
  mlib,
  lib,
  config,
  ...
}: let
  inherit (mlib) homeModule mkOpt;
  inherit (lib.types) listOf str;
  inherit (lib.lists) unique;
  inherit (lib.attrsets) mapAttrs' mapAttrsToList filterAttrs;
  inherit (builtins) filter replaceStrings;
in {
  options = {
    meow.workstation.waybarDiskFilter =
      mkOpt (listOf str)
      (["/boot"] ++ lib.lists.optional config.meow.impermanence.enable "/")
      {
        description = "Mountpoints to filter out in waybar.";
      };
  };

  config = homeModule {
    programs.waybar.settings = let
      diskName = name: replaceStrings ["/"] ["_"] name;

      disks' =
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
        config.fileSystems;

      # FIXME: think about this when you aren't sick
      filterList =
        map (mount: "disk#${diskName config.fileSystems.${mount}.device}")
        config.meow.workstation.waybarDiskFilter;

      filterF = disk:
        (filter (d: disk == d) filterList) == [];

      disks = filterAttrs (n: _: filterF n) disks';
    in [
      ({
          layer = "top";
          position = "top";
          height = 30;
          modules-left = ["hyprland/workspaces" "hyprland/window"];
          modules-center = ["clock"];
          modules-right =
            ["hyprland/language" "idle_inhibitor"]
            ++ filter filterF (unique (mapAttrsToList (_: fs: "disk#${replaceStrings ["/"] ["_"] fs.device}")
                config.fileSystems))
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
  };
}
