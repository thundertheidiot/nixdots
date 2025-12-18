{
  config,
  lib,
  mlib,
  ...
}: let
  cfg = config.meow.emacs;

  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf;
in {
  options = {
    meow.emacs = {
      enable = mkEnOpt "Install and configure emacs.";

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
    meow.searx.enable = true;
    # search for llms

    home-manager.sharedModules = [
      {
        meowEmacs.enable = cfg.enable;
      }
    ];
  };
}
