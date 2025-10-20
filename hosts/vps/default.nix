{
  config,
  lib,
  mlib,
  modulesPath,
  server,
  pkgs,
  ...
}: let
  inherit (lib) mkForce head;
in {
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
    ./authentik.nix
    ./disko.nix
    ./jellyfin.nix
    ./meowdzbot.nix
    ./secrets
    ./sodexobot.nix
    ./website.nix
    ./wireguard.nix
  ];

  config = {
    _module.args = rec {
      server.homeServer = "10.100.0.2";
    };

    system.stateVersion = "25.05";
    time.timeZone = "Europe/Helsinki";

    users.users.root.hashedPassword = "!";
    hardware.enableRedistributableFirmware = false;

    # org sync git
    users.users.orgsync = {
      isNormalUser = true;
      description = "Sync org roam git repository";

      home = "/${config.meow.impermanence.persist}/orgsync";
      createHome = true;

      shell = "${pkgs.git}/bin/git-shell";

      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBKwHM/9spQfyeNIl/p8N8XBuoKj8UrhuhhlbEwkrgjZ thunder@disroot.org"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE076iY58F/FczOv+ct8OKh2ByMaxN/a7+52rjdF6Vmv" # orgzly
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMKvFyn0+PoSl4PWZPyg2A+feiC3wgYElw+p2MPLTmy u0_a298@localhost" # termux
      ];
    };

    meow = {
      workstation.enable = false;
      home.enable = false;

      impermanence.enable = true;
      impermanence.persist = "/nix/persist";

      ssh.key = false;
      ssh.rootKey = true;

      gpu = "none";

      server = {
        publicSSH = true;
        webserver = true;

        deploy = {
          enable = true;
          pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIH43IsqYdCbqrJ6M00a7idsUJSC7hS96Nj3FQOxa355r";
        };

        domains = ["kotiboksi.xyz" "gooptyland.xyz" "saatana.xyz"];
        mainDomain = "kotiboksi.xyz";
        reverseProxy = {
          "img.${config.meow.server.mainDomain}" = "http://${server.homeServer}:2283";
        };

        certificates = ["kotiboksi.xyz"];
        xmppDomains = ["kotiboksi.xyz" "gooptyland.xyz"];
        coturn = true;
        mumble = true;
        radio = true;
        vaultwarden = true;
      };
    };

    mailserver.stateVersion = 3;

    networking.enableIPv6 = true;
    networking.interfaces.enp1s0.ipv6.addresses = [
      {
        address = "2a01:4f9:c010:9973::1";
        prefixLength = 64;
      }
    ];
    networking.defaultGateway6 = {
      address = "fe80::1";
      interface = "enp1s0";
    };
    networking.networkmanager.enable = false;
    networking.useDHCP = true;
    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

    programs.nh.clean.extraArgs = mkForce "--keep 3";
  };
}
