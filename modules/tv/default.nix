{
  config,
  lib,
  mlib,
}: let
  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf;

  cfg = config.meow.tv;
in {
  options = {
    meow.tv.enable = mkEnOpt "Enable smart-tv configuration";
  };

  config =
    mkIf cfg.enable {
    };
}
