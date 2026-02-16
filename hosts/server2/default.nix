{
  pkgs,
  config,
  lib,
  mlib,
  inputs,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib.types) str;
in {
  options = {
    # here for easier changing in case of router change etc.
    server.addr = mkOpt str "192.168.101.111" {};
  };

  imports = [
    # local self signed certificates
    (import ../../certs).module
    ./disko.nix
    ./dns.nix
    ./docker
    ./home-assistant
    ./homepage.nix
    ./jellyfin.nix
    ./redlib.nix
    ./secrets
    ./urlwatch.nix
    ./wireguard.nix
    inputs.autoaspm.nixosModules.default
  ];

  config = {
    system.stateVersion = "26.05";
    time.timeZone = "Europe/Helsinki";
    networking.hostName = "server2";

    powerManagement.powertop.enable = true;

    services.autoaspm.enable = true;

    hardware.graphics.enable = true;
    hardware.graphics.extraPackages = with pkgs; [
      intel-media-driver
      intel-vaapi-driver
      libvdpau-va-gl
    ];

    networking.networkmanager.enable = true;
    networking.firewall.allowedTCPPorts = [80 443];

    systemd.targets.sleep.enable = false;
    systemd.targets.suspend.enable = false;
    systemd.targets.hibernate.enable = false;
    systemd.targets.hybrid-sleep.enable = false;

    services.nginx = {
      enable = true;
      logError = "stderr debug";
    };

    meow = {
      workstation.enable = false;
      home.enable = false;

      boot.enable = false;

      impermanence.enable = true;
      impermanence.persist = "/nix/persist";

      gpu = "intel";

      ssh.key = false; # there is no user
      ssh.rootKey = true;
    };

    boot.initrd.availableKernelModules = ["xhci_pci" "ehci_pci" "ahci" "usbhid" "usb_storage" "sd_mod"];
    boot.initrd.kernelModules = ["i915"];
    boot.kernelModules = ["kvm-intel"];
    boot.extraModulePackages = [];
    boot.kernel.sysctl = {
      "fs.inotify.max_user_watches" = "1048576";
    };

    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
    hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };
}
