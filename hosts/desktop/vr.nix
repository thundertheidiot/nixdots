# Incredible resource
# https://lvra.gitlab.io
{
  inputs,
  config,
  lib,
  mlib,
  pkgs,
  ...
}:
let
  inherit (mlib) homeModule;
  inherit (lib) getExe;

  monadoI686 =
    (pkgs.pkgsi686Linux.monado.overrideAttrs (old: {
      cmakeFlags = (old.cmakeFlags or [ ]) ++ [
        "-DXRT_MODULE_MERCURY_HANDTRACKING=OFF"
        "-DXRT_BUILD_DRIVER_HANDTRACKING=OFF"
        "-DXRT_HAVE_ONNXRUNTIME=OFF"
      ];
    })).override
      {
        onnxruntime = pkgs.pkgsi686Linux.hello;
      };
in
{
  config = lib.mkMerge [
    # Monado
    {
      services.monado = {
        enable = true;
        defaultRuntime = true;
      };

      systemd.user.services.monado.environment = {
        HOME = config.meow.home.stubbornHomeDirectory;
        XRT_COMPOSITOR_SCALE_PERCENTAGE = "140";
        XRT_COMPOSITOR_COMPUTE = "0";
        U_PACING_COMP_MIN_TIME_MS = "4";
        STEAMVR_LH_ENABLE = "1";
        LH_DRIVER = "steamvr";
        U_PACING_APP_USE_MIN_FRAME_PERIOD = "1";
        WMR_HANDTRACKING = "0";
      };
    }
    # utils
    (homeModule {
      home.packages = with pkgs; [
        wayvr
      ];

      # xdg.configFile."wlxoverlay/openxr_actions.json5".text = lib.generators.toJSON [
      #   {
      #     profile = "/interaction_profiles/htc/vive_controller";
      #     pose = {
      #       left = "/user/hand/left/input/aim/pose";
      #       right = "/user/hand/right/input/aim/pose";
      #     };
      #     click = {
      #       left = "/user/hand/left/input/trigger/value";
      #       right = "/user/hand/right/input/trigger/value";
      #     };
      #     grab = {
      #       left = "/user/hand/left/input/squeeze/click";
      #       right = "/user/hand/right/input/squeeze/click";
      #     };
      #     scroll = {
      #       left = "/user/hand/left/input/trackpad/y";
      #       right = "/user/hand/right/input/trackpad/y";
      #     };
      #     show_hide = {
      #       left = "/user/hand/left/input/system/click";
      #     };
      #     space_drag = {
      #       right = "/user/hand/right/input/system/click";
      #     };
      #     haptic = {
      #       left = "/user/hand/left/output/haptic";
      #       right = "/user/hand/right/output/haptic";
      #     };
      #   }
      # ];

      xdg.configFile."wayvr/conf.d/config.yaml".source = (pkgs.formats.yaml { }).generate "config.yaml" {
        desktop_view_scale = 2.0;
        timezones = [ "Europe/Helsinki" ];
        notification_topics = {
          System = "Center";
          DesktopNotification = "Hide";
          XSNotification = "Hide";
          IpdChange = "Center";
        };
      };
    })
    {
      services.ananicy = {
        extraRules = [
          {
            "name" = "monado";
            "nice" = -20;
          }
          {
            "name" = "VRChat.exe";
            "nice" = -20;
          }
        ];
      };

      environment.systemPackages = [
        (pkgs.writeShellApplication {
          name = "vrhelper";

          # excludeShellChecks = [
          #   "SC2143"
          # ];

          runtimeInputs = [
            pkgs.inotify-tools
          ];

          text =
            let
              enable_vr_mode = pkgs.writeShellApplication {
                name = "enable_vr_mode";
                runtimeInputs = [
                  pkgs.fd
                ];
                text = ''
                  # find card with the power performance file (skips igpu)
                  card="$(fd --absolute-path --type symlink 'card[0-9]$' /sys/class/drm -x sh -c 'test -f "{}/device/pp_power_profile_mode" && echo "{}"' | head -n 1)"


                  [ "$(cat "$card/device/power_dpm_force_performance_level")" = "manual" ] && \
                    grep -q ' VR\*' "$card/device/pp_power_profile_mode" && exit 0

                  echo "manual" > "/sys/class/drm/$card/device/power_dpm_force_performance_level"

                  vr_profile=$(cat "/sys/class/drm/$card/device/pp_power_profile_mode" | grep ' VR ' | awk '{ print $1; }')
                  echo "$vr_profile" > "/sys/class/drm/$card/device/pp_power_profile_mode"
                '';
              };

              disable_vr_mode = pkgs.writeShellApplication {
                name = "enable_vr_mode";
                runtimeInputs = [
                  pkgs.fd
                ];
                text = ''
                  # find card with the power performance file (skips igpu)
                  card="$(fd --absolute-path --type symlink 'card[0-9]$' /sys/class/drm -x sh -c 'test -f "{}/device/pp_power_profile_mode" && echo "{}"' | head -n 1)"

                  echo "auto" > "/sys/class/drm/$card/device/power_dpm_force_performance_level"
                  echo 0 > "/sys/class/drm/$card/device/pp_power_profile_mode"
                '';
              };
            in
            ''
              [ -z "$1" ] && { echo "provide argument"; exit 1; }

              case $1 in
                game)
                  shift 1

                  exec env PRESSURE_VESSEL_FILESYSTEMS_RW="$XDG_RUNTIME_DIR/monado_comp_ipc" \
                           PRESSURE_VESSEL_IMPORT_OPENXR_1_RUNTIMES=1 \
                           XRT_COMPOSITOR_SCALE_PERCENTAGE=120 \
                           "$@"
                  ;;
                steam)
                  sudo "${getExe enable_vr_mode}" || true
                  ln -f "$XDG_CONFIG_HOME/openxr/1/steamvr_active_runtime.json" "$XDG_CONFIG_HOME/openxr/1/active_runtime.json"
                  ln -f "$XDG_CONFIG_HOME/openvr/steamvr_openvrpaths.vrpath" "$XDG_CONFIG_HOME/openvr/openvrpaths.vrpath"

                  steam steam://rungameid/250820
                  ;;
                monado)
                  sudo "${getExe enable_vr_mode}" || true
                  ln -f "$XDG_CONFIG_HOME/openxr/1/monado_active_runtime.json" "$XDG_CONFIG_HOME/openxr/1/active_runtime.json"
                  ln -f "$XDG_CONFIG_HOME/openxr/1/monado32_active_runtime.json" "$XDG_CONFIG_HOME/openxr/1/active_runtime.i686.json"
                  case "''${2:-}" in
                    "")
                      ln -f "$XDG_CONFIG_HOME/openvr/monado_xrizer_openvrpaths.vrpath" \
                            "$XDG_CONFIG_HOME/openvr/openvrpaths.vrpath"
                      ;;
                    open32)
                      ln -f "$XDG_CONFIG_HOME/openvr/monado_opencomposite32_openvrpaths.vrpath" \
                            "$XDG_CONFIG_HOME/openvr/openvrpaths.vrpath"
                      ;;
                    *)
                      ln -f "$XDG_CONFIG_HOME/openvr/monado_opencomposite_openvrpaths.vrpath" \
                            "$XDG_CONFIG_HOME/openvr/openvrpaths.vrpath"
                      ;;
                  esac

                  { sleep 10; wayvr; } &

                  # steam is placed in stubbornHome, this needs to be set so monado can find the steamvr stuff
                  env HOME=${config.meow.home.stubbornHomeDirectory} \
                      XRT_COMPOSITOR_SCALE_PERCENTAGE=120 \
                      XRT_COMPOSITOR_COMPUTE=0 \
                      U_PACING_COMP_MIN_TIME_MS=4 \
                      STEAMVR_LH_ENABLE=1 \
                      LH_DRIVER=steamvr \
                      U_PACING_APP_USE_MIN_FRAME_PERIOD=1 \
                      WMR_HANDTRACKING=0 \
                      monado-service
                  ;;
                disable)
                  sudo ${disable_vr_mode}
                  ;;
                *)
                  echo "invalid argument"
                  ;;
              esac
            '';
        })
      ];
    }
    # OpenXR and OpenVR files
    (homeModule (
      { config, ... }: {
        xdg.configFile."openxr/1/monado_active_runtime.json".text = builtins.toJSON {
          file_format_version = "1.0.0";
          runtime = {
            name = "Monado";
            library_path = "${pkgs.monado}/lib/libopenxr_monado.so";
          };
        };

        xdg.configFile."openxr/1/monado32_active_runtime.json".text = builtins.toJSON {
          file_format_version = "1.0.0";
          runtime = {
            name = "Monado";
            library_path = "${monadoI686}/lib/libopenxr_monado.so";
          };
        };

        xdg.configFile."openxr/1/steamvr_active_runtime.json".text = builtins.toJSON {
          file_format_version = "1.0.0";
          runtime = {
            VALVE_runtime_is_steamvr = true;
            name = "SteamVR";
            library_path = "${config.xdg.dataHome}/Steam/steamapps/common/SteamVR/bin/linux64/vrclient.so";
          };
        };

        xdg.configFile."openvr/monado_opencomposite_openvrpaths.vrpath".text = builtins.toJSON {
          config = [
            "${config.xdg.dataHome}/Steam/config"
          ];
          external_drivers = null;
          jsonid = "vrpathreg";
          log = [
            "${config.xdg.dataHome}/Steam/logs"
          ];
          runtime = [
            "${
              pkgs.opencomposite.overrideAttrs (old: {
                postInstall = ''
                  cp ${pkgs.pkgsi686Linux.opencomposite}/lib/opencomposite/bin/vrclient.so $out/lib/opencomposite/bin
                '';
              })
            }/lib/opencomposite"
          ];
          version = 1;
        };

        xdg.configFile."openvr/monado_opencomposite32_openvrpaths.vrpath".text = builtins.toJSON {
          config = [
            "${config.xdg.dataHome}/Steam/config"
          ];
          external_drivers = null;
          jsonid = "vrpathreg";
          log = [
            "${config.xdg.dataHome}/Steam/logs"
          ];
          runtime = [
            "${pkgs.pkgsi686Linux.opencomposite}/lib/opencomposite"
          ];
          version = 1;
        };

        xdg.configFile."openvr/monado_xrizer_openvrpaths.vrpath".text = builtins.toJSON {
          config = [
            "${config.xdg.dataHome}/Steam/config"
          ];
          external_drivers = null;
          jsonid = "vrpathreg";
          log = [
            "${config.xdg.dataHome}/Steam/logs"
          ];
          runtime = [
            "${
              pkgs.xrizer.overrideAttrs (old: {
                postInstall = lib.strings.concatStrings [
                  old.postInstall
                  ''
                    cp ${pkgs.pkgsi686Linux.xrizer}/lib/xrizer/bin/vrclient.so $out/lib/xrizer/bin
                  ''
                ];
              })
            }/lib/xrizer"
          ];
          version = 1;
        };

        xdg.configFile."openvr/steamvr_openvrpaths.vrpath".text = builtins.toJSON {
          config = [
            "${config.xdg.dataHome}/Steam/config"
          ];
          external_drivers = null;
          jsonid = "vrpathreg";
          log = [
            "${config.xdg.dataHome}/Steam/logs"
          ];
          runtime = [
            "${config.xdg.dataHome}/Steam/steamapps/common/SteamVR"
          ];
          version = 1;
        };
      }
    ))
  ];
}
