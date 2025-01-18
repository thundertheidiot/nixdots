{
  config,
  lib,
  mlib,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib.types) str;

  cfg = config.server;
in {
  options = {
    # here for easier changing in case of router change etc.
    server.addr = mkOpt str "192.168.101.101" {};
  };

  imports = [
    ./disko.nix
    ./dns.nix
    ./docker
    ./homepage.nix
    ./jellyfin.nix
    ./rathole.nix
    ./vaultwarden.nix
    ./webserver.nix
  ];

  config = {
    system.stateVersion = "24.11";
    time.timeZone = "Europe/Helsinki";
    networking.hostName = "uwu";

    nix.gc = {
      automatic = true;
      dates = "daily";
      options = "--delete-older-than 7d";
    };

    # networking.firewall.allowedTCPPorts = [
    #   6443
    # ];
    # networking.firewall.allowedUDPPorts = [
    # ];
    # services.k3s = {
    #   enable = true;
    #   role = "server";
    #   extraFlags = [
    #     "--disable=traefik"
    #   ];
    # };

    meow = {
      workstation.enable = false;
      shell.enable = true;

      ssh.key = false; # no user to give to
      ssh.rootKey = true;

      gpu = "intel";

      impermanence.enable = true;
      impermanence.persist = "/nix/persist";
      impermanence.directories = [
        {
          path = "/var/lib/rancher";
        }
      ];

      home.enable = false;
    };

    boot.initrd.availableKernelModules = ["xhci_pci" "ehci_pci" "ahci" "usbhid" "usb_storage" "sd_mod"];
    boot.initrd.kernelModules = [];
    boot.kernelModules = ["kvm-intel"];
    boot.extraModulePackages = [];
    boot.kernel.sysctl = {
      "fs.inotify.max_user_watches" = "1048576";
    };

    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
    hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };
}
