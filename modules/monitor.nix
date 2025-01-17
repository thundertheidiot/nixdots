{
  config,
  mlib,
  lib,
  pkgs,
  ...
}: let
  mons = mlib.mkMonitors config.meow.monitors;

  inherit (lib) mkIf mkMerge;
  inherit (mlib) homeModule;
in {
  options = let
    inherit (mlib) mkOpt;
    inherit (lib.types) listOf attrs;
  in {
    meow.monitors = mkOpt (listOf attrs) [] {
      example = [
        {
          name = "DP-3";
          width = "2560";
          height = "1440";
          refresh = "144";
          x = "1920";
        }
        {
          name = "DP-1";
          width = "1920";
          height = "1080";
          refresh = "144";
        }
      ];
    };
  };

  config = let
    inherit (builtins) filter isPath length elem;

    edids = filter (s: s != {}) (map (
        s:
          with s;
            if isPath edid
            then {inherit name edid;}
            else {}
      )
      mons);
  in
    mkMerge [
      (mkIf ((length edids) != 0) {
        hardware.firmware = map (s:
          with s; (pkgs.runCommandNoCC "${name}-edid-override" {compressFirmware = false;} ''
            mkdir -p "$out/lib/firmware/edid"
            cp "${edid}" "$out/lib/firmware/edid/${name}.bin"
          ''))
        edids;

        boot.kernelParams =
          map (
            s: with s; "drm.edid_firmware=${name}:edid/${name}.bin"
          )
          edids;

        environment.systemPackages = [
          pkgs.mpkgs.cru
        ];
      })
      {
        services.xserver.xrandrHeads = map (m:
          with m; {
            output =
              if (xorgName != "")
              then xorgName
              else name;

            inherit primary;
            monitorConfig = ''
              ${xorgModeline}
              ${
                if (xorgModeline != "")
                then let
                  inherit (lib.strings) splitString concatStrings;
                  inherit (lib.lists) sublist;
                  inherit (builtins) filter;

                  split' = splitString " " xorgModeline;
                  split = filter (s: s != "") split'; # get rid of empty entries caused by double spacing

                  name = concatStrings (sublist 1 1 split);
                in ''Option "PreferredMode" ${name}''
                else ""
              }
              Option "Position" "${x} ${y}"
            '';
          })
        mons;
      }
      (
        mkIf (elem "hyprland" config.meow.workstation.environment)
        (
          homeModule
          ({
            config,
            lib,
            pkgs,
            inputs,
            mlib,
            ...
          }: let
            # splitMonitorWorkspaces = (builtins.length mons) > 1;
            splitMonitorWorkspaces = true; # TODO remove balls
            hyprWorkspace =
              if splitMonitorWorkspaces
              then "split:workspace"
              else "workspace";
            hyprMoveToWorkspaceSilent =
              if splitMonitorWorkspaces
              then "split:movetoworkspacesilent"
              else "movetoworkspacesilent";

            inherit (builtins) filter isList length;

            monitorsWithModes = map (m: {
              inherit (m) name x y customModes;
            }) (filter (m: isList m.customModes) mons);

            modeSwitcher =
              pkgs.writers.writeBash "modemenu"
              (let
                inherit (lib.strings) concatStringsSep;
                inherit (builtins) listToAttrs head;

                monitorSwitchScripts = listToAttrs (map
                  (m: rec {
                    inherit (m) name x y customModes;
                    value = pkgs.writers.writeBash "${name}_modemenu" ''
                      pos=${x}x${y}

                      case $(echo -e "Disable\n${concatStringsSep "\n" (map (mode: mode.display) customModes)}" | tofi --prompt-text "Select mode: ") in
                        "Disable")
                          hyprctl keyword monitor "${name}, disabled"
                          ;;
                        ${concatStringsSep "\n" (map (m: with m; ''"${display}") hyprctl keyword monitor "${name}, ${real}, $pos, 1" ;;'') customModes)}
                      esac
                    '';
                  })
                  monitorsWithModes);
              in
                if (length monitorsWithModes == 1)
                then "${monitorSwitchScripts."${(head monitorsWithModes).name}"}"
                else ''
                  case $(echo -e "${concatStringsSep "\n" (map (m: m.name) monitorsWithModes)}" | tofi --prompt-text "Select monitor: ") in
                  ${concatStringsSep "\n" (map (m: with m; "\"${name}\") ${monitorSwitchScripts.${name}} ;;") monitorsWithModes)}
                  esac
                '');
          in {
            wayland.windowManager.hyprland = {
              plugins = lib.mkIf splitMonitorWorkspaces [pkgs.hyprsplit];

              settings = {
                plugin.split-monitor-workspaces = {
                  count = 10;
                  keep_focused = 1;
                };

                monitor = lib.mkIf (mons != []) (builtins.map (m:
                  with m;
                    lib.mkIf (!hyprlandExclude)
                    "${name}, ${width}x${height}@${refresh}, ${x}x${y}, ${scale}${
                      if (hyprlandExtra != "")
                      then ", ${hyprlandExtra}"
                      else ""
                    }")
                mons);

                workspace = lib.mkIf (!splitMonitorWorkspaces && mons != []) (
                  builtins.map (n: let
                    mon = (builtins.head mons).name;
                  in "${builtins.toString n}, monitor:${mon}")
                  [1 2 3 4 5 6 7 8 9]
                );

                bind = [
                  (lib.mkIf (builtins.length monitorsWithModes > 0) "$mod, C, exec, ${modeSwitcher}")

                  "$mod, 1, ${hyprWorkspace}, 1"
                  "$mod, 2, ${hyprWorkspace}, 2"
                  "$mod, 3, ${hyprWorkspace}, 3"
                  "$mod, 4, ${hyprWorkspace}, 4"
                  "$mod, 5, ${hyprWorkspace}, 5"
                  "$mod, 6, ${hyprWorkspace}, 6"
                  "$mod, 7, ${hyprWorkspace}, 7"
                  "$mod, 8, ${hyprWorkspace}, 8"
                  "$mod, 9, ${hyprWorkspace}, 9"
                  "$mod, 0, ${hyprWorkspace}, 0"

                  "$shiftmod, 1, ${hyprMoveToWorkspaceSilent}, 1"
                  "$shiftmod, 2, ${hyprMoveToWorkspaceSilent}, 2"
                  "$shiftmod, 3, ${hyprMoveToWorkspaceSilent}, 3"
                  "$shiftmod, 4, ${hyprMoveToWorkspaceSilent}, 4"
                  "$shiftmod, 5, ${hyprMoveToWorkspaceSilent}, 5"
                  "$shiftmod, 6, ${hyprMoveToWorkspaceSilent}, 6"
                  "$shiftmod, 7, ${hyprMoveToWorkspaceSilent}, 7"
                  "$shiftmod, 8, ${hyprMoveToWorkspaceSilent}, 8"
                  "$shiftmod, 9, ${hyprMoveToWorkspaceSilent}, 9"
                  "$shiftmod, 0, ${hyprMoveToWorkspaceSilent}, 0"
                ];
              };
            };
          })
        )
      )
    ];
}
