{
  pkgs,
  config,
  lib,
  mlib,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib.types) str;
in {
  options = {
    # here for easier changing in case of router change etc.
    server.addr = mkOpt str "192.168.101.101" {};
  };

  imports = [
    # ./home-assistant.nix
    # local self signed certificates
    (import ../../certs).module
    ./authentik.nix
    ./camera.nix
    ./disko.nix
    ./dns.nix
    ./docker
    ./forgejo.nix
    ./homepage.nix
    ./jellyfin.nix
    ./n8n.nix
    ./redlib.nix
    ./secrets
    ./wireguard.nix
  ];

  config = {
    system.stateVersion = "24.11";
    time.timeZone = "Europe/Helsinki";
    networking.hostName = "uwu";

    hardware.graphics.enable = true;
    hardware.graphics.extraPackages = with pkgs; [
      intel-media-driver
      intel-vaapi-driver
      libvdpau-va-gl
    ];

    networking.networkmanager.enable = true;
    networking.firewall.allowedTCPPorts = [80 443];

    services.nginx = {
      enable = true;
      logError = "stderr debug";
    };

    meow = {
      workstation.enable = false;
      home.enable = false;

      impermanence.enable = true;
      impermanence.persist = "/nix/persist";
      impermanence.directories = [
        {
          path = "/var/lib/rancher";
        }
      ];

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
