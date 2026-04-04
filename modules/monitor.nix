{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkIf mkMerge mkOption floor;
  inherit (lib.attrsets) attrValues;
  inherit (lib.lists) length elem filter sublist;
  inherit (lib.strings) splitString concatStrings concatStringsSep;

  inherit (lib.types) attrsOf listOf submodule str int float bool nullOr path;

  ifElseEmpty = t: v:
    if t
    then v
    else "";

  ifNotNull = t: ifElseEmpty (t != null);

  cfg = config.meow.monitors;
in {
  options = {
    meow.monitors = mkOption {
      description = "monitors";
      default = {};
      type = attrsOf (submodule ({name, ...}: {
        options = {
          name = mkOption {
            type = str;
            default = name;
            description = "Monitor name";
          };

          enable = mkOption {
            type = bool;
            default = true;
            description = "Enable this output";
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
            type = nullOr float;
            default = null;
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

          niriCustom = mkOption {
            type = bool;
            default = false;
            description = "Declare mode as custom in niri";
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
        mkIf (elem "niri" config.meow.workstation.environment) {
          meow.workstation.extraNiriConfig = map (mon: let
            inherit (mon) name width height scale x y;

            refresh = ifNotNull mon.refresh "@${toString mon.refresh}";
          in ''
            output "${name}" {
              mode ${ifElseEmpty mon.niriCustom "custom=true"} "${toString width}x${toString height}${refresh}"
              ${ifNotNull mon.primary "focus-at-startup"}
              scale ${toString scale}
              position x=${toString x} y=${toString y}
              ${ifElseEmpty (!mon.disableVrr) "variable-refresh-rate"}
            }
          '') (attrValues cfg);
        }
      )

      (
        mkIf (config.meow.emacs.ewm.enable) {
          meow.home.modules = [
            {
              xdg.configFile."emacs/ewm-local.el".text = ''
                (setq ewm-output-config '(${concatStringsSep "\n" (map (mon: let
                  inherit (mon) name width height scale x y;

                  emacsBool = bool:
                    if bool
                    then "t"
                    else "nil";
                in ''
                  ("${name}" :width ${toString width} :height ${toString height}
                             :scale ${toString scale}
                             :x ${toString x}
                             ${ifNotNull mon.refresh ":refresh ${toString (floor mon.refresh)}"}
                             :y ${toString y}
                             :enabled ${emacsBool mon.enable})'') (attrValues cfg))}))
              ''; # no vrr support yet
            }
          ];
        }
      )
    ];
}
