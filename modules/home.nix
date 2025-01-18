# This fun abstraction module lets me mix NixOS and home-manager options in a single module
# I'm pretty sure this is entirely useless though, as I think i can just `home-manager.users.user.imports = [module];'
# Oh well, it's already baked into much of the config
# TODO: get rid of the home module
{
  lib,
  pkgs,
  config,
  mlib,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib) mkIf;
  inherit (lib.types) bool listOf raw attrs str;
  inherit (lib.options) literalExpression;

  cfg = config.meow.home;
in {
  options = {
    meow.home = {
      enable = mkOpt bool true {};

      extraSpecialArgs = mkOpt attrs {inherit mlib;} {
        example = literalExpression "{ inherit inputs; }";
      };

      sharedModules = mkOpt (listOf raw) [] {};
      modules = mkOpt (listOf raw) [] {};

      user = mkOpt str config.meow.user {};
      stateVersion = mkOpt str "CHANGE" {};
      directory = mkOpt str "/home/${cfg.user}" {};

      file = mkOpt attrs {} {description = "Files to place in $HOME";};
      configFile = mkOpt attrs {} {description = "Files to place in $XDG_CONFIG_HOME";};
      dataFile = mkOpt attrs {} {description = "Files to place in $XDG_DATA_HOME";};
    };
  };
  config = mkIf cfg.enable {
    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
      backupFileExtension = "hm_backup";

      extraSpecialArgs = cfg.extraSpecialArgs;

      sharedModules = cfg.sharedModules;

      users.${cfg.user} = {
        home = {
          username = cfg.user;
          homeDirectory = cfg.directory;
          stateVersion = cfg.stateVersion;
        };

        imports = cfg.modules;

        home.file = cfg.file;
        xdg.configFile = cfg.configFile;
        xdg.dataFile = cfg.dataFile;
      };
    };
  };
}
