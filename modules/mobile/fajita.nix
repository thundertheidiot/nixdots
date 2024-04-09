{ config, inputs, pkgs, lib,... }: {
  config = {
    system.stateVersion = "24.05";
    
    mobile.boot.stage-1.kernel.useStrictKernelConfig = true;
    mobile.boot.stage-1.networking.enable = true;
    mobile.beautification = {
      silentBoot = true;
      splash = true;
      useKernelLogo = false;
    };

    nixpkgs.config.allowUnfree = true;

    users.users.thunder = {
      isNormalUser = true;
      initialHashedPassword = "$6$NYCdqJbUfedbL.mz$0BH6.WX0gkCpZf2CEpJ/CSkr2oGHEPH2mUtWutjvep4FaUncwrQJbuwMKpkJy5NJ40m8q.e1KVbKwE2afJxh5.";
      home = "/home/thunder";
      extraGroups = [
        "dialout"
        "feedbackd"
        "networkmanager"
        "video"
        "wheel"
      ];
      uid = 1000;
    };

    users.users.root.initialHashedPassword = "$6$osxDQOg5K1r0NEK6$nscJkxVQyktlmDUZaQGivwNsvsnnCQ2OK8kWpgPxKQNZatVQoNsP.xhmY9OQVWx97Hx6jXbkkiBKOxmnWgQsO1";
    users.users.thunder.openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBKwHM/9spQfyeNIl/p8N8XBuoKj8UrhuhhlbEwkrgjZ thunder@disroot.org"
    ];

    networking.wireless.enable = false; #wpa_supplicant
    networking.networkmanager = {
      enable = true;
      unmanaged = ["rndis0" "usb0"];
    };

    services.openssh = {
      enable = true;
      settings = {
        PermitRootLogin = "no";
        PasswordAuthentication = false;
      };
    };

    services.xserver = {
      enable = true;

      desktopManager.plasma5.mobile.enable = true;

      displayManager.autoLogin = {
        enable = true;
        user = "thunder";
      };

      displayManager.defaultSession = "plasma-mobile";

      displayManager.lightdm = {
        enable = true;
        # Workaround for autologin only working at first launch.
        # A logout or session crashing will show the login screen otherwise.
        extraSeatDefaults = ''
          session-cleanup-script=${pkgs.procps}/bin/pkill -P1 -fx ${pkgs.lightdm}/sbin/lightdm
        '';
      };

      libinput.enable = true;
    };

    hardware.bluetooth.enable = true;
    hardware.pulseaudio.enable = lib.mkForce false;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
    };
    powerManagement.enable = true;
  };
}
