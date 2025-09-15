{
  config,
  lib,
  pkgs,
  modulesPath,
  mlib,
  inputs,
  ...
}: {
  imports = [
    ./disko.nix
    inputs.nixos-hardware.nixosModules.framework-13-7040-amd
    ./kmod.nix # nixos-hardware workaround
  ];

  system.stateVersion = "25.05";

  time.timeZone = "Europe/Helsinki";
  networking.hostName = "framework";

  # networking.networkmanager.wifi.backend = "iwd";

  users.users.thunder = {
    initialPassword = "password";
    extraGroups = ["docker"];
  };

  powerManagement.enable = true;
  powerManagement.powertop.enable = true;

  services.fprintd = {
    enable = true;
    # tod.enable = true;
    # tod.driver = pkgs.libfprint-2-tod1-goodix;
  };

  services.power-profiles-daemon.enable = lib.mkForce false;
  services.tlp = {
    enable = true;
    settings = {
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

      CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
      CPU_ENERGY_PERF_POLICY_ON_AC = "performance";

      CPU_MIN_PERF_ON_AC = 0;
      CPU_MAX_PERF_ON_AC = 100;
      CPU_MIN_PERF_ON_BAT = 0;
      CPU_MAX_PERF_ON_BAT = 100;

      STOP_CHARGE_THRESH_BAT0 = 90;
    };
  };

  # security.pki.certificates = [
  #   ''
  #     -----BEGIN CERTIFICATE-----
  #     MIIDdzCCAl+gAwIBAgIQZqyAKU9rZpVPfh7Yfh2LxTANBgkqhkiG9w0BAQUFADBC
  #     MRIwEAYKCZImiZPyLGQBGRYCZmkxFzAVBgoJkiaJk/IsZAEZFgdjZW50cmlhMRMw
  #     EQYDVQQDEwpjZW50cmlhLUNBMB4XDTE1MTIwODEwMDMwMVoXDTQwMTIwODEwMTMw
  #     MFowQjESMBAGCgmSJomT8ixkARkWAmZpMRcwFQYKCZImiZPyLGQBGRYHY2VudHJp
  #     YTETMBEGA1UEAxMKY2VudHJpYS1DQTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCC
  #     AQoCggEBAJ5oNRf1Q7SMuZOCTqz0NWx/6mL7HinOmAYxWtX0MolZvumGUEVD711g
  #     wjmD4n2rgFz6j/afNA6EM7a+tgFDnWNwSxzZkopqu5pkif9/kWu1edqbyHmJRAC3
  #     ul8mUQUSdWaAW/oGM8CkGqozg3F/lusUsohoVhpkWwVNEhZSBtu8Z/rissPA7pXA
  #     gG9yhpr28CfwW9Rg64Gq2tDQtxr+xVt7lYVJe/dCggtvyBgTn9AFe/NCiocGfqOw
  #     47y2Q0M/NpE1tnsFhyzWpspmE0236F3Z2qfDwopQsNgjr0mT1atPJpYKOHfBhT1l
  #     GtIDVizqB7v/feryBWXOjAVnFOMgPMECAwEAAaNpMGcwEwYJKwYBBAGCNxQCBAYe
  #     BABDAEEwDgYDVR0PAQH/BAQDAgGGMA8GA1UdEwEB/wQFMAMBAf8wHQYDVR0OBBYE
  #     FNe8NC6s4083zu5iFogjHtbrEeTfMBAGCSsGAQQBgjcVAQQDAgEAMA0GCSqGSIb3
  #     DQEBBQUAA4IBAQArJVsKfE/TEI3qypF/VevKFQmbutGj4IdnziREDg//ROtIltnj
  #     21eQScPWIvMlxPhponjohgXYG8MaRL7n02e6ugxVRVlG4J2BtmBsa+mdjceWcGwj
  #     J3n3RuxDAjIyxbwhJ/gC3JaBBKJ9AEcgcwhlf7fa7+4KAVfjhHmNS+ACrGmm2kef
  #     6NHlou4vDw7toCmOJ1s6AJhy7z8mp54NOm0FJEeNXdEMvujcTHdMTixXjbse6FWK
  #     PjT5a0LdlUY2ZdYbskECp4nPm83LeKppPv13Quq/YZAtHvFXwDCDgEN3bMFEaMwY
  #     Wlyku1axVbOOXygANEc0ueAZZHXzyERKB+B6
  #     -----END CERTIFICATE-----
  #   ''
  # ];

  virtualisation.docker.enable = true;

  home-manager.sharedModules = [
    {
      home.stateVersion = "25.05";
      mHome.browser.firefox.enable = true;
      mHome.setup.fullLanguages = true;

      wayland.windowManager.hyprland.settings.xwayland.force_zero_scaling = true;

      home.packages = with pkgs; [
        vscode
        lmath
        distrobox

        python313Packages.python
      ];
    }
  ];

  services.cpupower-gui.enable = true;

  # virtualisation.virtualbox.host.enable = true;
  # users.extraGroups.vboxusers.members = ["thunder"];
  # boot.kernelPackages = lib.mkForce pkgs.linuxPackages_latest;

  meow = {
    fullSetup = true;
    workstation.enable = true;
    workstation.environment = ["hyprland"];
    workstation.displayManager = "gdm";

    impermanence.enable = true;

    user = "thunder";

    gaming.enable = true;
    # gaming.emulation = true;
    # gaming.games = ["duckgame"];

    emacs.enable = true;
    shell.enable = true;

    ssh.rootKey = true;

    gpu = "intel";

    virtualization.enable = true;

    keyboard = {
      enable = true;
      devices = ["/dev/input/by-path/platform-i8042-serio-0-event-kbd"];
    };

    boot.efi = true;

    monitors = [
      {
        name = "eDP-1";
        width = "2880";
        height = "1920";
        refresh = 120;
        primary = true;
        scale = "1.5";
      }
    ];
  };

  boot.loader.grub.enable = true;

  # boot.initrd.availableKernelModules = ["xhci_pci" "ehci_pci" "ahci" "usb_storage" "sd_mod" "sr_mod" "rtsx_pci_sdmmc"];
  # boot.initrd.kernelModules = [];
  # boot.kernelModules = ["kvm-intel"];
  # boot.extraModulePackages = [];

  networking.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
