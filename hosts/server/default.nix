{
  options = {
    workstation.enable = false;
    workstation.utils = "none";
    workstation.environment = [];
  };

  system = {
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
      ./homepage.nix
      ./jellyfin.nix
      ./rathole.nix
      ./torrent
      ./vaultwarden.nix
      ./webserver.nix
    ];

    config = {
      system.stateVersion = "24.11";
      time.timeZone = "Europe/Helsinki";
      networking.hostName = "server2";

      nix.gc = {
        automatic = true;
        dates = "daily";
        options = "--delete-older-than 7d";
      };

      users.users.thunder.initialPassword = "password";

      meow = {
        workstation.enable = false;
        shell.enable = true;

        ssh.rootKey = true;

        gpu = "intel";

        impermanence.enable = true;
        impermanence.persist = "/nix/persist";
        impermanence.directories = [
          {
            path = "/var/lib/rancher";
          }
        ];

        home = {
          stateVersion = "24.05";
        };
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
  };
}
