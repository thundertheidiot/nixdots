{
  config,
  lib,
  mlib,
  ...
}:
let
  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf mkForce;
  cfg = config.meow.workstation.audio.enable;
in
{
  options = {
    meow.workstation.audio.enable = mkEnOpt "Enable audio configuration.";
  };

  config = mkIf cfg {
    meow.impermanence.directories = [
      { path = "/var/lib/bluetooth"; permissions = "0700"; }
    ];

    hardware.bluetooth = {
      enable = true;
      powerOnBoot = true;
      settings = {
        # Needed for disabling hardware volume
        General.Experimental = true;
      };
    };

    # Forcefully disable pulseaudio
    services.pulseaudio.enable = mkForce false;

    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;

      wireplumber.extraConfig = {
        "10-disable-bluetooth-hw-volume"."monitor.bluez.properties" = {
          "bluez5.enable-hw-volume" = false;
        };
      };
    };
  };
}
