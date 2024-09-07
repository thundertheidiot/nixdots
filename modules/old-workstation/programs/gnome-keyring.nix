{
  system = {
    config,
    pkgs,
    lib,
    ...
  }:
    lib.mkIf (config.workstation.enable) {
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
    };

  home = {
    config,
    pkgs,
    lib,
    ...
  }:
    lib.mkIf (config.workstation.enable) {
      home.packages = with pkgs; [
        seahorse
        gnome-keyring
      ];

      services.gnome-keyring = {
        enable = true;
        components = ["pkcs11" "secrets" "ssh"];
      };

      services.gpg-agent = {
        enable = true;
        pinentryPackage = pkgs.pinentry-gnome3;
      };
    };
}
