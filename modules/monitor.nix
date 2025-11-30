{
  config,
  mlib,
  lib,
  pkgs,
  inputs,
  ...
}: let
  mons = inputs.self.lib.mkMonitors config.meow.monitors;

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
          with s; (pkgs.runCommand "${name}-edid-override" {compressFirmware = false;} ''
            mkdir -p "$out/lib/firmware/edid"
            cp "${edid}" "$out/lib/firmware/edid/${name}.bin"
          ''))
        edids;

        boot.kernelParams =
          map (
            s: with s; "drm.edid_firmware=${name}:edid/${name}.bin"
          )
          edids;

        # environment.systemPackages = [
        #   pkgs.mpkgs.cru
        # ];
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
              plugins = [pkgs.hyprsplit];

              settings = {
                plugin.hyprsplit = {
                  num_workspaces = 9;
                  persistent_workspaces = true;
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

                bind = [
                  (lib.mkIf (builtins.length monitorsWithModes > 0) "$mod, C, exec, ${modeSwitcher}")

                  "$mod, 1, split:workspace, 1"
                  "$mod, 2, split:workspace, 2"
                  "$mod, 3, split:workspace, 3"
                  "$mod, 4, split:workspace, 4"
                  "$mod, 5, split:workspace, 5"
                  "$mod, 6, split:workspace, 6"
                  "$mod, 7, split:workspace, 7"
                  "$mod, 8, split:workspace, 8"
                  "$mod, 9, split:workspace, 9"

                  "$shiftmod, 1, split:movetoworkspacesilent, 1"
                  "$shiftmod, 2, split:movetoworkspacesilent, 2"
                  "$shiftmod, 3, split:movetoworkspacesilent, 3"
                  "$shiftmod, 4, split:movetoworkspacesilent, 4"
                  "$shiftmod, 5, split:movetoworkspacesilent, 5"
                  "$shiftmod, 6, split:movetoworkspacesilent, 6"
                  "$shiftmod, 7, split:movetoworkspacesilent, 7"
                  "$shiftmod, 8, split:movetoworkspacesilent, 8"
                  "$shiftmod, 9, split:movetoworkspacesilent, 9"
                ];
              };
            };
          })
        )
      )
    ];
}
