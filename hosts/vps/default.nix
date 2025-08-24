{
  config,
  lib,
  mlib,
  ...
}: let
  inherit (lib) mkForce;
in {
  config = {
    system.stateVersion = "25.05";
    time.timeZone = "Europe/Helsinki";

    meow = {
      workstation.enable = false;
      home.enable = false;

      impermanence.enable = true;
      impermanence.persist = "/nix/persist";

      ssh.key = false;
      ssh.rootKey = true;
    };

    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

    programs.nh.clean.extraArgs = mkForce "--keep 3";

    networking = {
      hostName = "vps";
      networkmanager.enable = false;

      interfaces."eth0" = {
        useDHCP = false;
        ipv4.addresses = [
          {
            address = "185.26.51.191";
            prefixLength = 22;
          }
        ];
      };

      interfaces."eth1" = {
        useDHCP = false;
        ipv4.addresses = [
          {
            address = "10.1.14.178";
            prefixLength = 22;
          }
        ];
      };

      interfaces."eth2" = {
        useDHCP = false;
        ipv6.addresses = [
          {
            address = "2a04:3540:1000:310:60e3:52ff:fee6:1e37";
            prefixLength = 64;
          }
        ];
      };

      defaultGateway = {
        interface = "eth0";
        address = "185.26.48.1";
      };

      defaultGateway6 = {
        interface = "eth2";
        address = "fe80::987e:ff:fe8d:c967";
      };

      # Cloudflare DNS over IPv4 and IPv6
      nameservers = [
        "1.1.1.1"
        "1.0.0.1"
        "2606:4700:4700::1111"
        "2606:4700:4700::1001"
      ];
    };
  };
}
