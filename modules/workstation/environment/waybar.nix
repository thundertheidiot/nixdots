{
  mlib,
  lib,
  config,
  pkgs,
  ...
}: let
  inherit (mlib) homeModule mkOpt;
  inherit (lib) mkIf;
  inherit (lib.types) listOf str attrsOf anything;
  inherit (lib.lists) unique elem;
  inherit (lib.attrsets) mapAttrs' mapAttrsToList filterAttrs attrNames;
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

    meow.workstation.waybarDiskFormat = mkOpt str "{path} {free}" {
      description = "Set the format for disks.";
    };

    meow.workstation.extraWaybarModules = mkOpt (attrsOf anything) {} {
      description = "Extra modules to add to waybar.";
    };
  };

  config = mkIf (work && (elem "niri" env)) (homeModule {
    programs.waybar.settings = let
      diskName = name:
        if name == null
        then "NULL"
        else replaceStrings ["/"] ["_"] name;

      filterList =
        ["/boot"]
        ++ lib.lists.optional config.meow.impermanence.enable "/"
        ++ config.meow.workstation.waybarDiskFilter;

      fileSystems =
        filterAttrs (_: d: !elem d.mountPoint filterList) config.fileSystems;

      disks =
        mapAttrs' (n: v: {
          name = "disk#${diskName v.device}";
          value = {
            interval = 30;
            format = config.meow.workstation.waybarDiskFormat;
            path = n;
            warning = 80;
            critical = 90;

            on-click = "xdg-open '${n}'";
          };
        })
        fileSystems;
    in {
      main =
        {
          modules-left = [];
          modules-center = ["clock"];

          "group/system" = {
            orientation = "horizontal";
            modules = ["power-profiles-daemon" "custom/swaync" "pulseaudio" "battery" "tray"];
          };

          modules-right =
            unique (mapAttrsToList (_: fs: "disk#${replaceStrings ["/"] ["_"] fs.device}")
              fileSystems)
            ++ (attrNames config.meow.workstation.extraWaybarModules)
            ++ ["network"]
            ++ ["group/system"];

          clock = {
            interval = 1;
            timezone = "${config.time.timeZone}";
            format = "{:%a, %d %b  %H:%M}";
          };

          "custom/swaync" = {
            tooltip = false;
            "return-type" = "json";
            exec = "swaync-client -swb";
            "on-click" = "swaync-client -t -sw";
            "on-click-right" = "swaync-client -d -sw";
            escape = true;
          };

          pulseaudio = {
            scroll-step = 3;
            escape = true;
            on-click = "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
          };

          battery = {
            states = {
              warning = 30;
              critical = 15;
            };
          };

          tray = {spacing = 10;};
        }
        // disks
        // config.meow.workstation.extraWaybarModules;
    };
  });
}
