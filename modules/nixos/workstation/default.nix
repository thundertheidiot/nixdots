{
  lib,
  config,
  pkgs,
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
      xorg.xhost
      gparted
      wireguard-tools

      distrobox
    ];

    virtualisation.docker.enable = true;

    networking.firewall.checkReversePath = false;
    
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

    security.polkit.enable = true;
    systemd.user.services.polkit-gnome-authentication-agent-1 = {
      description = "polkit-gnome-authentication-agent-1";
      wantedBy = ["graphical-session.target"];
      wants = ["graphical-session.target"];
      after = ["graphical-session.target"];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 10;
      };
    };

    hardware.bluetooth = {
      enable = true;
      powerOnBoot = true;
      settings = {
        General.Experimental = true;
      };
    };

    services.xserver.enable = true;

    services.getty = {
      helpLine = "";
      extraArgs = ["--noclear" "-n" "-o" "${config.username}"];
    };

    boot.kernelPackages = pkgs.linuxPackages_cachyos;

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
