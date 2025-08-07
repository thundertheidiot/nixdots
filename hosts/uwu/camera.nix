{
  pkgs,
  lib,
  ...
}: {
  config = {
    systemd.services."motion" = let
      inherit (builtins) isString isInt;

      ini = globalSection:
        lib.generators.toINIWithGlobalSection {
          mkKeyValue = lib.generators.mkKeyValueDefault {
            mkValueString = v:
              if v == true
              then "on"
              else if v == false
              then "off"
              else if isString v
              then "${v}"
              else if isInt v
              then "${toString v}"
              else lib.generators.mkValueDefault {} v;
          } " ";
        } {
          inherit globalSection;
        };

      config = pkgs.writeText "config.ini" (ini {
        stream_localhost = false;
        stream_port = 9874;
        framerate = 12;
        minimum_frame_time = 0;
        minimum_motion_frames = 3;
        target_dir = "/mnt/storage/camera";
      });
    in {
      enable = true;
      description = "motion to watch camera for motion";
      unitConfig = {
        Type = "simple";
      };
      script = ''
        mkdir --parents /mnt/storage/camera
        ${pkgs.motion}/bin/motion -c ${config}
      '';
      wantedBy = ["multi-user.target"];
    };

    networking.firewall = {
      allowedTCPPorts = [9874];
      allowedUDPPorts = [9874];
    };
  };
}
