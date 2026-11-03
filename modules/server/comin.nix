{
  config,
  lib,
  mlib,
  ...
}:
let
  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf;

  cfg = config.meow.server.comin;
in
{
  options.meow.server = {
    comin.enable = mkEnOpt "Enable comin deployment";
  };

  config = mkIf cfg.enable {
    meow.impermanence.directories = [ "/var/lib/comin" ];

    services.comin = {
      enable = true;
      remotes = [
        {
          name = "origin";
          url = "https://github.com/thundertheidiot/nixdots.git";
          branches.main.name = "main";
        }
      ];
    };
  };
}
