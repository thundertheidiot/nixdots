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
            ["hyprland/language" "idle_inhibitor"]
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
      c = config.lib.stylix.colors;
      # Choose an accent from your Stylix scheme. base0D is a good “blue” default.
      accent = colors.base0D;
    in ''
      /* Base typography */
      * {
        border: none;
        font-family: Inter, System-ui, "JetBrainsMono Nerd Font", "Symbols Nerd Font", sans-serif;
        font-size: 13.5px;
        font-weight: 500;
        min-height: 0;
      }

      /* Float the bar; let compositor blur it if enabled */
      window#waybar {
        background: transparent;
        margin: 10px 14px 0 14px;
        color: ${colors.base05};
      }

      /* Glassy container */
      window#waybar > box {
        background: rgba(${c.base00-rgb-r}, ${c.base00-rgb-g}, ${c.base00-rgb-b}, 0.38);
        border: 1px solid ${colors.base02};
        border-radius: 14px;
        box-shadow:
          0 18px 40px rgba(0,0,0,0.35),
          0 2px 6px rgba(0,0,0,0.22);
        padding: 6px;
      }

      /* Even spacing between areas */
      .modules-left, .modules-center, .modules-right {
        margin: 0 4px;
      }

      /* Modern pill modules */
      .modules-left > widget > *,
      .modules-center > widget > *,
      .modules-right > widget > * {
        background: rgba(${c.base01-rgb-r}, ${c.base01-rgb-g}, ${c.base01-rgb-b}, 0.70);
        border: 1px solid ${colors.base02};
        border-radius: 12px;
        padding: 4px 10px;
        margin: 0 4px;
        transition: background 160ms ease, border-color 160ms ease, color 160ms ease, box-shadow 160ms ease;
      }

      .modules-left > widget > *:hover,
      .modules-center > widget > *:hover,
      .modules-right > widget > *:hover {
        background: rgba(${c.base02-rgb-r}, ${c.base02-rgb-g}, ${c.base02-rgb-b}, 0.80);
        border-color: ${colors.base03};
        box-shadow: inset 0 0 0 1px rgba(255,255,255,0.04);
      }

      /* Workspaces: minimal dots with accent for active */
      #workspaces,
      #workspaces:hover {
        padding: 2px 6px;
        background: rgba(${c.base01-rgb-r}, ${c.base01-rgb-g}, ${c.base01-rgb-b}, 0.70);
        border: 1px solid ${colors.base02};
        /* optional: keep it visually calm */
        box-shadow: inset 0 0 0 1px rgba(255,255,255,0.04);
      }

      #workspaces button label {
        font-size: 0;
        padding: 0;
        margin: 0;
      }

      #workspaces button {
        all: unset;
        min-width: 18px;
        min-height: 18px;
        padding: 0;
        margin: 0 4px;
        border-radius: 999px;

        background: transparent;
        border: 1px solid ${colors.base02};

        box-shadow: inset 0 0 0 2px rgba(255,255,255,0.10);

        transition: background 120ms ease, border-color 120ms ease;
      }

      #workspaces button:hover {
        background: rgba(255,255,255,0.06);
        margin-top: -1px;
        margin-bottom: 1px;
      }

      /* Empty workspaces: just a hollow ring */
      #workspaces button.empty {
        opacity: 0.7;
        box-shadow: inset 0 0 0 2px rgba(255,255,255,0.16);
      }

      /* Occupied workspaces: show a center dot (via inset box-shadow) */
      #workspaces button:not(.empty) {
        /* first value creates the center dot; second keeps a faint inner ring */
        box-shadow:
          inset 0 0 0 5px rgba(255,255,255,0.16);
        opacity: 1;
      }

      /* Active workspace: fully filled with accent */
      #workspaces button.active {
        background: transparent;
        border-color: rgba(255,255,255,0.22);
        box-shadow:
          inset 0 0 0 5px ${accent},                 /* solid accent dot */
          inset 0 0 0 2px rgba(255,255,255,0.14);    /* faint inner ring */
        opacity: 1;
      }

      /* Urgent stays obvious */
      #workspaces button.urgent {
        background: ${colors.base08};
        border-color: rgba(255,255,255,0.25);
      }

      /* Center title: muted, truncated */
      #window {
        color: ${colors.base04};
        min-width: 380px;
      }

      /* State coloring to make it feel “alive” */
      #pulseaudio.muted { color: ${colors.base04}; }
      #network.disconnected { color: ${colors.base04}; }
      #battery.warning { color: ${colors.base0A}; }  /* warn */
      #battery.critical { color: ${colors.base08}; } /* crit */

      /* Tray looks cohesive */
      #tray { padding: 2px 6px; }
      #tray > .passive { opacity: 0.8; }
      #tray > .needs-attention {
        color: ${colors.base08};
        animation: blink 1s ease-in-out infinite alternate;
      }
      @keyframes blink { from { opacity: 1 } to { opacity: .5 } }

      /* Tooltips styled like the bar */
      tooltip {
        background: rgba(${c.base00-rgb-r}, ${c.base00-rgb-g}, ${c.base00-rgb-b}, 0.90);
        border: 1px solid ${colors.base02};
        border-radius: 10px;
      }
      tooltip label { color: ${colors.base05}; }
    '';
  });
}
