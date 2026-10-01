{
  config,
  lib,
  mlib,
  ...
}:
let
  cfg = config.meow.emacs;

  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf;
in
{
  options = {
    meow.emacs = {
      enable = mkEnOpt "Install and configure emacs.";
      ewm.enable = mkEnOpt "Configure ewm.";
    };
  };

  config = mkIf cfg.enable {
    # search for llms
    meow.searx.enable = true;

    programs.ewm.enable = cfg.ewm.enable;

    home-manager.sharedModules = [
      {
        meowEmacs.enable = cfg.enable;
      }
    ];
  };
}
