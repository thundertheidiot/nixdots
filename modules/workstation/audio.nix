{
  config,
  lib,
  mlib,
  inputs,
  ...
}: let
  inherit (mlib) mkEnOptTrue;
  inherit (lib) mkIf mkForce;
  cfg = config.meow.workstation.audio;
in {
  options = {
    meow.workstation.audio = mkEnOptTrue "Enable audio configuration.";
  };

  imports = [
    inputs.nix-gaming.nixosModules.pipewireLowLatency
  ];

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
    hardware.pulseaudio.enable = mkForce false;

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

      lowLatency = {
        enable = true;
      };
    };
  };
}
