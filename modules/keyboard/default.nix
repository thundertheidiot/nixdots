{
  config,
  lib,
  mlib,
  ...
}: let
  cfg = config.meow.keyboard;
  inherit (mlib) mkOpt mkEnOpt;
  inherit (lib) mkIf;

  inherit (lib.types) listOf path str;
in {
  options = {
    meow.keyboard = {
      enable = mkEnOpt "Enable keyboard configuration through kanata";
      devices = mkOpt (listOf str) {} {
        example = lib.options.literalExpression ''[ "/dev/input/by-id/usb-YMDK_YD60MQ-if01-event-kbd" ]'';
        description = "Devices to apply kanata to";
      };

      layout = mkOpt str "" {};
    };
  };

  config = {
    services.kanata = mkIf cfg.enable {
      enable = true;

      keyboards.meow = {
        devices = cfg.devices;

        config = ''
          (defsrc
            esc
            grv  1    2    3    4    5    6    7    8    9    0    -     =    bspc
            tab  q    w    e    r    t    y    u    i    o    p    [     ]    ret
            caps a    s    d    f    g    h    j    k    l    ;    '     \
            lsft 102d z    x    c    v    b    n    m    ,    .    /     rsft
            lctl lmet lalt           spc            ralt cmps rctl up
                                                              left down rght
          )

          (defalias
            base (layer-switch base)
            nrm (layer-switch normal)
            raise (layer-while-held raise))

          (defalias
            ;;cwc (caps-word-custom-toggle
            ;;      2000
            ;;      (1 2 3 4 5 6 7 8 9 0 - = q w e r t y u i o p [ ] \ a s d f g h j k l ' z x c v b n m , . / )
            ;;      (bspc))
            esc (tap-hold-release 175 175 esc lmet)
            spc (tap-hold-release 175 175 spc (layer-while-held numpad))

            ;; home row mods
            a (tap-hold-release 175 175 a lmet)
            s (tap-hold-release 175 175 s lalt)
            d (tap-hold-release 175 175 d lsft)
            f (tap-hold-release 175 175 f lctl)

            j (tap-hold-release 175 175 j lctl)
            k (tap-hold-release 175 175 k rsft)
            l (tap-hold-release 175 175 l ralt)
            ; (tap-hold-release 175 175 ; rmet))

          (deflayer base
            esc
            grv  1    2    3    4    5    6    7    8    9    0    -     =    bspc
            tab  q    w    e    r    t    y    u    i    o    p    [     ]    ret
            @esc @a   @s   @d   @f   g    h    @j   @k   @l   @;   '     \
            lsft 102d z    x    c    v    b    n    m    ,    .    /     rsft
            lctl lmet lalt           @spc           @raise @base rctl up
                                                              left down rght
          )

          (deflayer normal
            esc
            grv  1    2    3    4    5    6    7    8    9    0    -     =    bspc
            tab  q    w    e    r    t    y    u    i    o    p    [     ]    ret
            @esc a    s    d    f    g    h    j    k    l    ;    '     \
            lsft 102d z    x    c    v    b    n    m    ,    .    /     rsft
            lctl lmet lalt           spc            _    _    rctl up
                                                              left down rght
          )

          (deflayer numpad
            _
            _    _    _    _    _    _    _    _    _    _    _    _     _     _
            _    _    _    _    _    _    _    7    8    9    _    _     _     _
            _    lmet lalt lsft lctl _    _    4    5    6    _    _     _
            _    _    _    _    _    _    _    _    1    2    3    _     _
            _    _    _              _              0    _    _    _
                                                              _    _    _
          )

          (deflayer raise
            XX
            XX   XX   XX   XX   XX   XX   XX   XX   XX   XX   XX   XX    XX    XX
            XX   XX   XX   XX   XX   XX   XX   XX   XX   XX   pp   XX    XX    XX
            XX   XX   prnt XX   XX   @nrm XX   XX   XX   XX   XX   XX    XX
            XX   XX   XX   XX   XX   XX   XX   XX   XX   XX   XX   XX    XX
            XX   XX   XX             XX             XX   XX   XX   XX
                                                              XX   XX   XX
          )

        '';
      };
    };
  };
}
