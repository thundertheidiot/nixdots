{
  config,
  lib,
  mlib,
  ...
}: let
  inherit (lib) mkIf mkMerge;
  inherit (mlib) mkEnOptTrue mkOpt;
  inherit (lib.types) str;
  cfg = config.meow.x11;
in {
  options = {
    # Configuring this doesn't necessarily enable the xserver, so this is fine to do by default
    meow.x11.enable = mkEnOptTrue "xserver configuration";
    meow.x11.xkb.layout = mkOpt str "us" {};
    meow.x11.xkb.options = mkOpt str "eurosign:e" {};

    meow.x11.opinionatedMouseConfig = mkEnOptTrue "opinionated pointer device configuration for x11";
  };

  config = mkMerge [
    (mkIf cfg.enable {
      services.xserver = {
        xkb.layout = cfg.xkb.layout;
        xkb.options = cfg.xkb.options;
      };
    })

    (mkIf cfg.opinionatedMouseConfig {
      services.xserver.config = lib.mkAfter ''
        Section "InputClass"
          Identifier "libinput pointer catchall"
          MatchIsPointer "on"
          MatchDevicePath "/dev/input/event*"
          Driver "libinput"
          Option "AccelProfile" "flat"
        EndSection

        Section "InputClass"
          Identifier "Touchpad"
          MatchIsTouchpad "on"
          Driver "libinput"
          Option "NaturalScrolling" "true"
        EndSection

        Section "InputClass"
          Identifier "Trackpoint Acceleration"
          MatchProduct "TPPS/2 IBM TrackPoint|DualPoint Stick|Synaptics Inc. Composite TouchPad / TrackPoint|ThinkPad USB Keyboard with TrackPoint|USB Trackpoint pointing device|Composite TouchPad / TrackPoint"
          Driver "libinput"
          Option "AccelProfile" "adaptive"
        EndSection
      '';
    })
  ];
}
