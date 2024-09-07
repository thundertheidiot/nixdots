{
  config,
  mlib,
  lib,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib) mkIf;
  inherit (lib.types) bool;
in {
  options = {
    meow.cleanup = mkOpt bool true {};
  };

  config = mkIf config.meow.cleanup {
    services.speechd.enable = lib.mkForce false;

    meow.home.modules = [
      {
        # notice config is from nixos, not hm
        home.sessionVariables = lib.mkForce config.environment.variables;
        systemd.user.sessionVariables = lib.mkForce config.environment.variables;
      }
      ({config, ...}: {
        xdg.configFile = {
          "wget/wgetrc".text = "hsts-file = \"$XDG_CACHE_HOME\"/wget-hsts";
          "npm/npmrc".text = ''
            prefix=${config.xdg.dataHome}/npm
            cache=${config.xdg.cacheHome}/npm
            init-module=${config.xdg.configHome}/npm/config/npm-init.js
          '';
        };
      })
    ];
  };
}
