{
  mlib,
  lib,
  config,
  pkgs,
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
          height = 36;
          spacing = 8;

          modules-left = ["hyprland/workspaces" "hyprland/window"];
          modules-center = ["clock"];
          modules-right =
            ["hyprland/language" "idle_inhibitor" "custom/swaync"]
            ++ unique (mapAttrsToList (_: fs: "disk#${replaceStrings ["/"] ["_"] fs.device}")
              fileSystems)
            ++ ["network" "pulseaudio" "battery" "tray"];

          "hyprland/workspaces" = {
            format = " ";
            disable-scroll = true;
            active-only = false;
            all-outputs = false;
            persistent-workspaces = {"*" = 9;};
          };

          "hyprland/window" = {
            format = "{title}";
            max-length = 48;
            separate-outputs = true;
            empty-format = "Desktop";
          };

          clock = {
            interval = 1;
            timezone = "${config.time.timeZone}";
            format = "{:%a, %d %b  %H:%M}";
          };

          "hyprland/language" = {
            format = " {short}";
            tooltip-format = "{long}";
          };

          idle_inhibitor = {
            format = "{icon}";
            format-icons = {
              activated = "";
              deactivated = "";
            };
            on-click = "hyprctl keyword misc:disable_autoreload 1"; # no-op you can replace
          };

          "custom/swaync" = {
            tooltip = false;
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
            "return-type" = "json";
            exec = "swaync-client -swb";
            "on-click" = "swaync-client -t -sw";
            "on-click-right" = "swaync-client -d -sw";
            escape = true;
          };

          network = {
            format-wifi = " {essid} ({signalStrength}%)";
            format-ethernet = "󰈀 {ifname}: {ipaddr}/{cidr}";
            format-linked = "No Internet ⚠";
            format-disconnected = "  Disconnected";
            tooltip-format = "{ifname}  {ipaddr}/{cidr}\n{gwaddr}";
          };

          pulseaudio = {
            scroll-step = 3;
            format = "{icon} {volume}%";
            format-muted = "󰝟 Muted";
            format-bluetooth = "{icon} {volume}% ";
            format-bluetooth-muted = " Muted ";
            format-icons = {default = ["" "" ""];};
            escape = true;
            on-click = "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
          };

          battery = {
            states = {
              warning = 30;
              critical = 15;
            };
            format = "{icon} {capacity}% ({time})";
            format-charging = " {capacity}% ({time})";
            format-plugged = " {capacity}%";
            format-alt = "{capacity}%";
            format-icons = ["" "" "" "" ""];
          };

          tray = {spacing = 10;};
        }
        // disks)
    ];

    # This is pretty much vibe coded, don't @ me
    programs.waybar.style = let
      colors = config.lib.stylix.colors.withHashtag;
    in
      pkgs.replaceVars ./waybar.css {
        accent = colors.base0D;

        fg = colors.base05;
        border = colors.base02;
        borderHover = colors.base03;
        muted = colors.base04;
        warn = colors.base0A;
        danger = colors.base08;

        inherit (colors) base00 base01 base02;
      };
  });
}
