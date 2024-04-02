{
  lib,
  config,
  pkgs,
  localconfig,
  inputs,
  ...
}: {
  imports = [
    inputs.nix-gaming.nixosModules.pipewireLowLatency
    ./hyprland.nix
    ./theming.nix
  ];
  config = lib.mkIf (config.setup.userMachine.enable) (with config; {
    environment.systemPackages = with pkgs; [
      dmenu
      dconf
      gnome.gnome-keyring
    ];

    services.gnome.gnome-keyring.enable = true;

    programs.seahorse.enable = true;

    security.pam.services.gnome-keyring = {
      name = "gnome-keyring";
      enableGnomeKeyring = true;
      text = ''
        auth optional ${pkgs.gnome.gnome-keyring}/lib/security/pam_gnome_keyring.so
        session optional ${pkgs.gnome.gnome-keyring}/lib/security/pam_gnome_keyring.so auto_start
        password optional ${pkgs.gnome.gnome-keyring}/lib/security/pam_gnome_keyring.so
      '';
    };

    services.xserver.enable = true;

    services.getty = {
      helpLine = "";
      extraArgs = ["--noclear" "-n" "-o" "${config.username}"];
    };

    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;

      lowLatency = {
        enable = true;
      };
    };

    # Takes like 5 seconds of extra time on boot
    systemd.services."NetworkManager-wait-online".enable = false;
    
    services.xserver.displayManager.startx.enable = true;

    services.xserver.displayManager.session = [
      (lib.mkIf (config.setup.awesomeWM.enable) {
        manage = "desktop";
        name = "awesome";
        start = ''
          ${inputs.nixpkgs-f2k.packages.${pkgs.system}.awesome-git}/bin/awesome &
          waitPID=$!
        '';
      })
    ];

    services.xserver.displayManager.sessionPackages = [
      (
        lib.mkIf (config.setup.hyprland.enable)
        pkgs.hyprland
      )
    ];
  });
}
