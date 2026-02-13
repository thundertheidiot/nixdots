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
    ./gatus.nix
    ./jellyfin.nix
    ./meowdzbot.nix
    ./secrets
    ./sodexobot.nix
    ./website.nix
    ./wireguard.nix
  ];

  config = {
    _module.args = {
      server.homeServer = "10.100.0.2";
      server.homeServer2 = "10.100.0.3";
    };

    networking.hostName = "vps";

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
        "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBE6bPM6n9EHBHhkFe9KZ6QrHnCtdCFWSZQp6OLDNlIhUz4YB1ZYMIB1E0iEyW0XiICOeWYcy/MoYCAnJafF+Xhs=" # orgzly
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

        # forgejo = {
        #   enable = true;
        #   domain = "git.kotiboksi.xyz";
        # };

        domains = ["kotiboksi.xyz" "meowcloud.net" "gooptyland.xyz" "saatana.xyz"];
        mainDomain = "kotiboksi.xyz";
        reverseProxy = {
          "img.${config.meow.server.mainDomain}" = "http://${server.homeServer}:2283";
          "vw.meowcloud.net" = "http://127.0.0.1:${toString config.services.vaultwarden.config.ROCKET_PORT}";
        };

        jellyfinDomains = ["jellyfin.kotiboksi.xyz" "jellyfin.meowcloud.net"];

        certificates = ["kotiboksi.xyz" "meowcloud.net"];
        xmppDomains = ["kotiboksi.xyz" "gooptyland.xyz"];
        coturn = true;
        mumble = true;
        radio.enable = true;
        vaultwarden = true;
      };
    };

    services.journald.extraConfig = ''
      SystemMaxUse=500M
      SystemKeepFree=200M
      MaxFileSec=7day
    '';

    mailserver.stateVersion = 3;

    networking.useNetworkd = true;
    systemd.network.enable = true;
    systemd.network.networks."30-wan" = {
      matchConfig.Name = "enp1s0";
      networkConfig.DHCP = "no";
      address = [
        "95.216.151.56/32"
        "2a01:4f9:c010:9973::/64"
      ];
      routes = [
        {
          Gateway = "172.31.1.1";
          GatewayOnLink = true;
        }
        {
          Gateway = "fe80::1";
        }
      ];
    };

    networking.enableIPv6 = true;
    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

    programs.nh.clean.extraArgs = mkForce "--keep 3";
  };
}
