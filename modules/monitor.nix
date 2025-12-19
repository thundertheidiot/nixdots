{
  config,
  mlib,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib) mkIf mkMerge mkOption;
  inherit (lib.attrsets) attrValues listToAttrs;
  inherit (lib.lists) head length elem filter sublist;
  inherit (lib.strings) splitString concatStrings concatStringsSep;
  inherit (mlib) homeModule mkOpt;

  inherit (lib.types) attrsOf attrs listOf submodule str int float bool nullOr path;

  cfg = config.meow.monitors;
in {
  options = {
    meow.monitors = mkOption {
      description = "monitors";
      type = attrsOf (submodule ({name, ...}: {
        options = {
          name = mkOption {
            type = str;
            default = name;
            description = "Monitor name";
          };

          xorgName = mkOption {
            type = str;
            default = name;
            description = "Monitor name for xorg";
          };

          xorgModeline = mkOption {
            type = str;
            default = "";
            description = "Modeline for xorg";
          };

          disableVrr = mkOption {
            type = bool;
            default = false;
            description = "Disable variable refresh rate";
          };

          width = mkOption {
            type = int;
            default = 1920;
            description = "Width of the monitor";
          };

          height = mkOption {
            type = int;
            default = 1080;
            description = "Height of the monitor";
          };

          refresh = mkOption {
            type = float;
            default = 60.0;
            description = "Refresh rate of the monitor";
          };

          x = mkOption {
            type = int;
            default = 0;
            description = "X position";
          };

          y = mkOption {
            type = int;
            default = 0;
            description = "Y position";
          };

          scale = mkOption {
            type = float;
            default = 1.0;
            description = "Monitor scale";
          };

          primary = mkOption {
            type = bool;
            default = false;
            description = "Is this the primary monitor";
          };

          edid = mkOption {
            type = nullOr path;
            default = null;
            description = "Custom edid binary";
          };

          hyprlandExtra = mkOption {
            type = str;
            default = "";
            description = "Extra config for hyprland";
          };

          hyprlandExclude = mkOption {
            type = bool;
            default = false;
            description = "Exclude from hyprland config";
          };

          customModes = mkOption {
            type = nullOr (listOf (submodule {
              options = {
                name = mkOption {
                  type = str;
                };

                mode = mkOption {
                  type = str;
                };
              };
            }));
            default = null;
            description = "Custom modes for mode menu";
          };
        };
      }));
    };
  };

  config = let
    edids =
      filter (m: m.edid != null) (attrValues cfg);
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
                  split' = splitString " " xorgModeline;
                  split = filter (s: s != "") split'; # get rid of empty entries caused by double spacing

                  name = concatStrings (sublist 1 1 split);
                in ''Option "PreferredMode" ${name}''
                else ""
              }
              Option "Position" "${toString x} ${toString y}"
            '';
          })
        (attrValues cfg);
      }
      (
        mkIf (elem "hyprland" config.meow.workstation.environment)
        (
          homeModule
          ({
            lib,
            pkgs,
            ...
          }: let
            monitorsWithModes = filter (m: m.customModes != null) (attrValues cfg);

            modeSwitcher =
              pkgs.writers.writeBash "modemenu"
              (let
                monitorSwitchScripts = listToAttrs (map
                  (m: rec {
                    inherit (m) name x y customModes;

                    value = pkgs.writers.writeBash "${name}_modemenu" ''
                      pos=${toString x}x${toString y}

                      case $(echo -e "Disable\n${concatStringsSep "\n" (map (mode: mode.name) customModes)}" | ${lib.getExe pkgs.tofi} --prompt-text "Select mode: ") in
                        "Disable")
                          hyprctl keyword monitor "${name}, disabled"
                          ;;
                        ${concatStringsSep "\n" (map (m: ''"${m.name}") hyprctl keyword monitor "${name}, ${m.mode}, $pos, 1" ;;'') customModes)}
                      esac
                    '';
                  })
                  monitorsWithModes);
              in
                if (length monitorsWithModes == 1)
                then "${monitorSwitchScripts."${(head monitorsWithModes).name}"}"
                else ''
                  case $(echo -e "${concatStringsSep "\n" (map (m: m.name) monitorsWithModes)}" | ${lib.getExe pkgs.tofi} --prompt-text "Select monitor: ") in
                  ${concatStringsSep "\n" (map (m: with m; "\"${name}\") ${monitorSwitchScripts.${name}} ;;") monitorsWithModes)}
                  esac
                '');
          in {
            wayland.windowManager.hyprland = {
              settings = {
                monitor = mkIf (cfg != {}) (map (m:
                  with m;
                    mkIf (!hyprlandExclude)
                    "${name}, ${toString width}x${toString height}@${toString refresh}, ${toString x}x${toString y}, ${toString scale}${
                      if disableVrr
                      then ", vrr, 0"
                      else ""
                    }${
                      if (hyprlandExtra != "")
                      then ", ${hyprlandExtra}"
                      else ""
                    }")
                (attrValues cfg));

                bind = [
                  (mkIf (length monitorsWithModes > 0) "$mod, C, exec, ${modeSwitcher}")
                ];
              };
            };
          })
        )
      )
    ];
}
