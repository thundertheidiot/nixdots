# Incredible resource
# https://lvra.gitlab.io
{
  config,
  lib,
  mlib,
  pkgs,
  ...
}: let
  inherit (mlib) homeModule;
in {
  config = lib.mkMerge [
    # SteamVR
    {
      # NOTE: "Security" concern
      # This patch allows any application to use the privileges granted by CAP_SYS_NICE
      # Could potentially lead to system lockups
      boot.kernelPatches = [
        # {
        #   name = "amdgpu-ignore-ctx-privileges";
        #   patch = pkgs.fetchpatch {
        #     name = "cap_sys_nice_begone.patch";
        #     url = "https://github.com/Frogging-Family/community-patches/raw/master/linux61-tkg/cap_sys_nice_begone.mypatch";
        #     hash = "sha256-Y3a0+x2xvHsfLax/uwycdJf3xLxvVfkfDVqjkxNaYEo=";
        #   };
        # }
      ];

      # systemd.services."steamvr-setcap" = {
      #   enable = false;
      #   description = "Run setcap to fix steamvr.";
      #   unitConfig.Type = "simple";
      #   serviceConfig = {
      #     ExecStart = "${pkgs.libcap}/bin/setcap CAP_SYS_NICE+ep ${config.homeDirectory}/.local/share/Steam/steamapps/common/SteamVR/bin/linux64/vrcompositor-launcher || true";
      #   };
      #   wantedBy = ["multi-user.target"];
      # };

      programs.corectrl.enable = true;
      # programs.corectrl.gpuOverclock.enable = true;

      programs.envision = {
        enable = true;
      };
    }
    # funy
    {
      # Old vrc launch option
      # env U_PACING_APP_USE_MIN_FRAME_PERIOD=1 PRESSURE_VESSEL_FILESYSTEMS_RW=$XDG_RUNTIME_DIR/monado_comp_ipc vrchatlauncher %command% --enable-avpro-in-proton
      environment.systemPackages = with pkgs; [
        (writeShellApplication {
          name = "vrchatlauncher";

          runtimeInputs = [
            inotify-tools
          ];

          text = ''
            steamapps=/home/thunder/.local/share/Steam/steamapps
            watch_folder="$steamapps"/compatdata/438100/pfx/drive_c/users/steamuser/AppData/LocalLow/VRChat/VRChat

            do_taskset() {
            	log=$(inotifywait --include '.*\.txt' --event create "$watch_folder" --format '%f')

            	echo "Log: $watch_folder/$log"

            	while ! pid=$(pgrep VRChat); do
            		sleep 0.1
            	done

            	echo "Setting VRChat to dual-core..."
            	taskset -pac 0,1 "$pid"

            	tail -f "$watch_folder/$log" 2>/dev/null | sed -n '/EOS Login Succeeded/{p;q}'
            	sleep 1

            	echo "Setting VRChat to all cores..."
            	taskset -pac "0-$(($(nproc) - 1))" "$pid"

            	echo "Our work here is done."
            }

            LD_PRELOAD=\'\' do_taskset </dev/null &
            exec "$@"
          '';
        })
      ];
    }
    # Envision
    # {
    #   programs.envision.enable = true;
    # }
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
    # wlx-overlay-s
    (homeModule {
      home.packages = with pkgs; [
        wlx-overlay-s
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

      xdg.configFile."wlxoverlay/conf.d/config.yaml".source = (pkgs.formats.yaml {}).generate "config.yaml" {
        desktop_view_scale = 2.0;
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

          text = let
            card = "card0";

            enable_vr_mode = pkgs.writeShellScript "enable_vr_mode" ''
              card="$(${pkgs.fd}/bin/fd --absolute-path --type symlink 'card[0-9]$' /sys/class/drm)"

              [ "$(cat $card/device/power_dpm_force_performance_level)" = "manual" ] && \
                [ "$(grep ' VR\*' $card/device/pp_power_profile_mode)" ] && exit 0

              echo "manual" > /sys/class/drm/${card}/device/power_dpm_force_performance_level

              vr_profile=$(cat /sys/class/drm/${card}/device/pp_power_profile_mode | grep ' VR ' | awk '{ print $1; }')
              echo $vr_profile > /sys/class/drm/${card}/device/pp_power_profile_mode
            '';

            disable_vr_mode = pkgs.writeShellScript "disable_vr_mode" ''
              echo "auto" > /sys/class/drm/${card}/device/power_dpm_force_performance_level
              echo 0 > /sys/class/drm/${card}/device/pp_power_profile_mode
            '';
          in ''
            [ -z "$1" ] && { echo "provide argument"; exit 1; }

            case $1 in
              game)
                shift 1

                exec env PRESSURE_VESSEL_FILESYSTEMS_RW="$XDG_RUNTIME_DIR/monado_comp_ipc" \
                         U_PACING_APP_USE_MIN_FRAME_PERIOD=1 \
                         U_PACING_COMP_MIN_TIME_MS=4 \
                         XRT_COMPOSITOR_SCALE_PERCENTAGE=120 \
                         "$@"
                ;;
              steam)
                sudo ${enable_vr_mode}
                ln -f "$XDG_CONFIG_HOME/openxr/1/steamvr_active_runtime.json" "$XDG_CONFIG_HOME/openxr/1/active_runtime.json"
                ln -f "$XDG_CONFIG_HOME/openvr/steamvr_openvrpaths.vrpath" "$XDG_CONFIG_HOME/openvr/openvrpaths.vrpath"

                steam steam://rungameid/250820
                ;;
              monado)
                sudo "${enable_vr_mode}" || true
                ln -f "$XDG_CONFIG_HOME/openxr/1/monado_active_runtime.json" "$XDG_CONFIG_HOME/openxr/1/active_runtime.json"
                if [ -n "$2" ]; then
                  ln -f "$XDG_CONFIG_HOME/openvr/monado_xrizer_openvrpaths.vrpath" "$XDG_CONFIG_HOME/openvr/openvrpaths.vrpath"
                else
                  ln -f "$XDG_CONFIG_HOME/openvr/monado_opencomposite_openvrpaths.vrpath" "$XDG_CONFIG_HOME/openvr/openvrpaths.vrpath"
                fi

                # { sleep 3; pkexec renice -20 -p $(pgrep monado); } &

                { sleep 10; wlx-overlay-s; } &

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
    (homeModule ({config, ...}: {
      xdg.configFile."openxr/1/monado_active_runtime.json".text = builtins.toJSON {
        file_format_version = "1.0.0";
        runtime = {
          name = "Monado";
          library_path = "${pkgs.monado}/lib/libopenxr_monado.so";
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
          "${pkgs.opencomposite}/lib/opencomposite"
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
          "${pkgs.xrizer}/lib/xrizer"
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
    }))
  ];
}
