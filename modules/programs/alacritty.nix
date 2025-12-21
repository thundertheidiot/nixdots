{
  config,
  mlib,
  lib,
  ...
}: let
  inherit (lib) mkIf;
  inherit (mlib) mkEnOptTrue;

  cfg = config.meow.program.alacrittyConfig;
in {
  options = {
    meow.program.alacrittyConfig = mkEnOptTrue "Configure alacritty";
  };

  config = mkIf cfg {
    meow.home.modules = [
      {
        # Note that this does not enable the program itself
      }
    ];
  };
}
