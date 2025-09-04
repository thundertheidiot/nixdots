{
  config,
  lib,
  pkgs,
  modulesPath,
  mlib,
  ...
}: {
  imports = [
    # ./desktop/firedragon.nix
    ./disko.nix
  ];

  system.stateVersion = "25.05";

  time.timeZone = "Europe/Helsinki";
  networking.hostName = "t440p";

  # virtualisation.docker.enable = true;

  meow.impermanence.enable = true;

  users.users.thunder = {
    initialPassword = "password";
    extraGroups = ["docker"];
  };

  security.pki.certificates = [
    ''
      -----BEGIN CERTIFICATE-----
      MIIDdzCCAl+gAwIBAgIQZqyAKU9rZpVPfh7Yfh2LxTANBgkqhkiG9w0BAQUFADBC
      MRIwEAYKCZImiZPyLGQBGRYCZmkxFzAVBgoJkiaJk/IsZAEZFgdjZW50cmlhMRMw
      EQYDVQQDEwpjZW50cmlhLUNBMB4XDTE1MTIwODEwMDMwMVoXDTQwMTIwODEwMTMw
      MFowQjESMBAGCgmSJomT8ixkARkWAmZpMRcwFQYKCZImiZPyLGQBGRYHY2VudHJp
      YTETMBEGA1UEAxMKY2VudHJpYS1DQTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCC
      AQoCggEBAJ5oNRf1Q7SMuZOCTqz0NWx/6mL7HinOmAYxWtX0MolZvumGUEVD711g
      wjmD4n2rgFz6j/afNA6EM7a+tgFDnWNwSxzZkopqu5pkif9/kWu1edqbyHmJRAC3
      ul8mUQUSdWaAW/oGM8CkGqozg3F/lusUsohoVhpkWwVNEhZSBtu8Z/rissPA7pXA
      gG9yhpr28CfwW9Rg64Gq2tDQtxr+xVt7lYVJe/dCggtvyBgTn9AFe/NCiocGfqOw
      47y2Q0M/NpE1tnsFhyzWpspmE0236F3Z2qfDwopQsNgjr0mT1atPJpYKOHfBhT1l
      GtIDVizqB7v/feryBWXOjAVnFOMgPMECAwEAAaNpMGcwEwYJKwYBBAGCNxQCBAYe
      BABDAEEwDgYDVR0PAQH/BAQDAgGGMA8GA1UdEwEB/wQFMAMBAf8wHQYDVR0OBBYE
      FNe8NC6s4083zu5iFogjHtbrEeTfMBAGCSsGAQQBgjcVAQQDAgEAMA0GCSqGSIb3
      DQEBBQUAA4IBAQArJVsKfE/TEI3qypF/VevKFQmbutGj4IdnziREDg//ROtIltnj
      21eQScPWIvMlxPhponjohgXYG8MaRL7n02e6ugxVRVlG4J2BtmBsa+mdjceWcGwj
      J3n3RuxDAjIyxbwhJ/gC3JaBBKJ9AEcgcwhlf7fa7+4KAVfjhHmNS+ACrGmm2kef
      6NHlou4vDw7toCmOJ1s6AJhy7z8mp54NOm0FJEeNXdEMvujcTHdMTixXjbse6FWK
      PjT5a0LdlUY2ZdYbskECp4nPm83LeKppPv13Quq/YZAtHvFXwDCDgEN3bMFEaMwY
      Wlyku1axVbOOXygANEc0ueAZZHXzyERKB+B6
      -----END CERTIFICATE-----
    ''
  ];

  virtualisation.docker.enable = true;

  home-manager.sharedModules = [
    {
      home.stateVersion = "25.05";
      mHome.browser.firefox.enable = true;
      mHome.setup.fullLanguages = true;

      home.packages = with pkgs; [
        vscode
        lmath
        distrobox
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

    user = "thunder";

    # gaming.enable = true;
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

    monitors = [
      {
        name = "eDP-1";
        width = "1920";
        height = "1080";
      }
    ];
  };

  boot.loader.grub.enable = true;

  boot.initrd.availableKernelModules = ["xhci_pci" "ehci_pci" "ahci" "usb_storage" "sd_mod" "sr_mod" "rtsx_pci_sdmmc"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-intel"];
  boot.extraModulePackages = [];

  networking.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
