{lib, ...}: let
in {
  config = {
    services.xserver = {
      xkb.layout = "us";
      xkb.options = "eurosign:e";
    };

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
  };
}
