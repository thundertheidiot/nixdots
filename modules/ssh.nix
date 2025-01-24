# do not import this module, unless you want a visitor on your machine
{
  config,
  lib,
  mlib,
  ...
}: let
  cfg = config.meow.ssh;

  inherit (mlib) mkOpt mkEnOpt;
  inherit (lib.types) bool;
in {
  options = {
    meow.ssh = {
      key = mkOpt bool true {description = "Insert *my* ssh key in authorizedKeys.";};
      rootKey = mkEnOpt "Also give root the key.";
    };
  };
  config = {
    services.openssh = {
      enable = true;
      settings = {
        PermitRootLogin = "yes";
        PasswordAuthentication = false;
      };
    };

    users.users = let
      key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBKwHM/9spQfyeNIl/p8N8XBuoKj8UrhuhhlbEwkrgjZ thunder@disroot.org";
      inherit (lib) mkIf mkMerge;
    in
      mkMerge [
        (mkIf cfg.rootKey {
          root.openssh.authorizedKeys.keys = [key];
        })
        # 'mkIf cfg.key [key]' tries to create the user
        (mkIf cfg.key {
          "${config.meow.user}".openssh.authorizedKeys.keys = [key];
        })
      ];
  };
}
