{ config, lib, mlib, ... }: let
  cfg = config.meow.kmonad;
  inherit (mlib) mkOpt mkEnOpt;
  inherit (lib) mkIf;

  inherit (lib.types) path str;
in {
  options = {
    meow.kmonad = {
      enable = mkEnOpt "Enable kmonad";
      device = mkOpt path "" {};

      layout = mkOpt str "" {};

    };
  };

  config = {
    services.kmonad = (mkIf cfg.enable {
      enable = true;
      
      keyboards = {
        "${cfg.device}" = {
          name = "yea";
          device = cfg.device;

          defcfg.enable = true;

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
            normal (layer-switch normal)
            raise (layer-toggle raise))

          (deflayer base
            esc
            grv  1    2    3    4    5    6    7    8    9    0    -     =    bspc
            tab  q    w    e    r    t    y    u    i    o    p    [     ]    ret
            caps a    s    d    f    g    h    j    k    l    ;    '     \
            lsft 102d z    x    c    v    b    n    m    ,    .    /     rsft
            lctl lmet lalt           spc            @raise @base rctl up
                                                              left down rght
          )

          (deflayer normal
            esc
            grv  1    2    3    4    5    6    7    8    9    0    -     =    bspc
            tab  q    w    e    r    t    y    u    i    o    p    [     ]    ret
            caps a    s    d    f    g    h    j    k    l    ;    '     \
            lsft 102d z    x    c    v    b    n    m    ,    .    /     rsft
            lctl lmet lalt           spc            _    _    rctl up
                                                              left down rght
          )

          (deflayer raise
            XX
            XX   XX   XX   XX   XX   XX   XX   XX   XX   XX   XX   XX    XX    XX 
            XX   XX   XX   XX   XX   XX   XX   XX   XX   XX   XX   XX    XX    XX 
            XX   XX   XX   XX   XX   @normal   XX   XX   XX   XX   XX   XX    XX
            XX   XX   XX   XX   XX   XX   XX   XX   XX   XX   XX   XX    XX
            XX   XX   XX             XX             XX   XX   XX   XX
                                                              XX   XX   XX
          )

      '';
        };
      };




      # config = if (cfg.layout == "") then
      #   throw "kmonad enabled but meow.kmonad.layout is not set"
      #          else
      #            cfg.layout;
    });
  };
}
