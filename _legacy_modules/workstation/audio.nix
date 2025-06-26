{
  config,
  lib,
  mlib,
  ...
}: let
  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf mkForce;
  cfg = config.meow.workstation.audio.enable;
in {
  options = {
    meow.workstation.audio.enable = mkEnOpt "Enable audio configuration.";
  };

  config = mkIf cfg {
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
        "monitor.bluez.properties" = {
          "bluez5.enable-hw-volume" = false;
        };
      };

      # nix-gaming
      lowLatency.enable = true;
    };
  };
}
