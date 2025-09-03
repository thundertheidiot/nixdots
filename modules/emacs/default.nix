{
  config,
  pkgs,
  lib,
  mlib,
  ...
}: let
  cfg = config.meow.emacs;

  inherit (mlib) mkEnOpt homeModule;
  inherit (lib) mkIf;
in {
  options = {
    meow.emacs = {
      enable = mkEnOpt "Install and configure emacs.";
      exwm = mkEnOpt "Install and configure EXWM.";

      # TODO: move all this shit to modules/langs or something
      lang = {
        latex = mkEnOpt "Latex support";
        haskell = mkEnOpt "Haskell";
        rust = mkEnOpt "Rust";
        ocaml = mkEnOpt "Ocaml";
        lua = mkEnOpt "Lua";
        fennel = mkEnOpt "Fennel";
        janet = mkEnOpt "Janet";
        lisp = mkEnOpt "Lisp";
        c_cxx = mkEnOpt "C/C++";
        bash = mkEnOpt "Bash";
        python = mkEnOpt "Python";
        javascript = mkEnOpt "Javascript";
      };
    };
  };

  config = mkIf cfg.enable {
    services.xserver.displayManager.session = lib.mkIf cfg.exwm [
      {
        manage = "desktop";
        name = "EXWM";
        start = ''
          export EMACS_ENABLE_EXWM=1
          export _JAVA_AWT_WM_NONREPARENTING=1
          exec ${pkgs.dbus}/bin/dbus-launch --exit-with-session emacs -mm &
          waitPID=$!
        '';
      }
    ];

    services.xserver.enable = lib.mkDefault cfg.exwm;
    services.xserver.displayManager.startx.enable = cfg.exwm;

    meow.searx.enable = true;

    home-manager.sharedModules = [
      {
        mHome.emacs.enable = cfg.enable;
        mHome.emacs.exwm = cfg.exwm;
      }
    ];
  };
}
