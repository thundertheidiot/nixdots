{pkgs, ...}: {
  config = {
    environment.systemPackages = with pkgs; [
      envision
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

    services.monado = {
      enable = true;
      defaultRuntime = true;
    };

    systemd.user.services.monado.environment = {
      STEAMVR_LH_ENABLE = "1";
      XRT_COMPOSITOR_COMPUTE = "1";
      WMR_HANDTRACKING = "0";
    };

    meow.home.modules = [
      ({config, ...}: {
        xdg.configFile."openxr/1/active_runtime.json".text = ''
          {
            "file_format_version": "1.0.0",
            "runtime": {
                "name": "Monado",
                "library_path": "${pkgs.monado}/lib/libopenxr_monado.so"
            }
          }
        '';

        xdg.configFile."openvr/openvrpaths.vrpath".text = ''
          {
            "config" :
            [
              "${config.xdg.dataHome}/Steam/config"
            ],
            "external_drivers" : null,
            "jsonid" : "vrpathreg",
            "log" :
            [
              "${config.xdg.dataHome}/Steam/logs"
            ],
            "runtime" :
            [
              "${pkgs.opencomposite}/lib/opencomposite"
            ],
            "version" : 1
          }
        '';
      })
    ];
  };
}
