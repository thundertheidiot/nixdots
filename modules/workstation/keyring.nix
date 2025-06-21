{
  pkgs,
  config,
  mlib,
  lib,
  ...
}: let
  inherit (lib) mkIf;
  inherit (mlib) mkEnOpt;

  cfg = config.meow.workstation.gnomeKeyring.enable;
in {
  options = {
    meow.workstation.gnomeKeyring.enable = mkEnOpt "Set up gnome keyring, disable this if you want kwallet for some reason?";
  };

  config = mkIf cfg {
    services.gnome.gnome-keyring.enable = true;
    programs.seahorse.enable = true;

    programs.ssh.askPassword = lib.mkForce "${pkgs.seahorse}/libexec/seahorse/ssh-askpass";

    security.pam.services.gnome-keyring = {
      name = "gnome-keyring";
      enableGnomeKeyring = true;
      text = ''
        auth optional ${pkgs.gnome-keyring}/lib/security/pam_gnome_keyring.so
        session optional ${pkgs.gnome-keyring}/lib/security/pam_gnome_keyring.so auto_start
        password optional ${pkgs.gnome-keyring}/lib/security/pam_gnome_keyring.so
      '';
    };

    environment.variables = {
      SSH_ASKPASS = "${pkgs.seahorse}/libexec/seahorse/ssh-askpass";
    };

    meow.home.modules = [
      {
        home.sessionVariables = {
          SSH_ASKPASS = "${pkgs.seahorse}/libexec/seahorse/ssh-askpass";
        };

        services.gnome-keyring = {
          enable = true;
          components = ["pkcs11" "secrets" "ssh"];
        };

        services.gpg-agent = {
          enable = true;
          pinentry.package = pkgs.pinentry-gnome3;
        };
      }
    ];
  };
}
