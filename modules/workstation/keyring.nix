{
  pkgs,
  config,
  mlib,
  lib,
  ...
}: let
  inherit (lib) mkIf;
  inherit (mlib) mkEnOptTrue;

  cfg = config.meow.workstation.gnomeKeyring.enable;
in {
  options = {
    meow.workstation.gnomeKeyring.enable = mkEnOptTrue "Set up gnome keyring, disable this if you want kwallet for some reason?";
  };

  config = mkIf cfg {
    services.gnome.gnome-keyring.enable = true;
    programs.ssh.enableAskPassword = true;

    programs.seahorse.enable = true;
    programs.ssh.askPassword = lib.mkForce "${pkgs.seahorse}/libexec/seahorse/ssh-askpass";

    meow.home.modules = [
      {
        services.gnome-keyring = {
          enable = true;
          components = ["pkcs11" "secrets" "ssh"];
        };

        # services.gpg-agent = {
        #   enable = true;
        #   pinentry.package = pkgs.pinentry-gnome3;
        # };
      }
    ];
  };
}
