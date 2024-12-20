{...}: {
  meow.sops.enableSecrets = [
    "server_airvpn_private"
    "server_airvpn_preshared"
  ];

  networking.nat = {
    enable = true;
    externalInterface = "eno1";
    internalInterfaces = ["ve-+"];
    enableIPv6 = true;
  };

  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = 1;
    "net.ipv6.conf.all.forwarding" = 1;
  };

  # meow.sops.secrets."server_airvpn_private".path = "/run/vpncontainer/private_key";
  # meow.sops.secrets."server_airvpn_preshared".path = "/run/vpncontainer/preshared_key";

  containers.vpnContainer = {
    autoStart = true;
    privateNetwork = true;
    enableTun = true;

    hostAddress = "192.168.10.1";
    localAddress = "192.168.10.2";
    hostAddress6 = "fc00::1";
    localAddress6 = "fc00::2";

    forwardPorts = [
      {
        protocol = "tcp";
        hostPort = 8080;
        containerPort = 8080;
      }
    ];

    ephemeral = true;
    bindMounts = {
      "secrets" = {
        mountPoint = "/run/secrets";
        hostPath = "/run/secrets";
      };
    };

    config = {
      config,
      pkgs,
      lib,
      ...
    }: {
      system.stateVersion = "24.11";

      environment.systemPackages = [pkgs.curl];

      services.resolved.enable = true;

      networking = {
        defaultGateway.address = "192.168.10.1";
        nameservers = ["1.1.1.1"];
        firewall.checkReversePath = false;

        useHostResolvConf = lib.mkForce false;

        interfaces."eth0".ipv4.routes = [
          {
            address = "192.168.10.0";
            prefixLength = 24;
            via = "192.168.10.1";
          }
        ];
      };

      systemd.services."wg-quick-airvpn-sweden" = {
        # path = [pkgs.bind.host];
        # preStart = "until host se3.vpn.airdns.org; do sleep 1; done";
        requires = ["systemd-resolved.service"];

        serviceConfig = {
          Restart = lib.mkForce "on-failure";
          RestartSec = lib.mkForce "1s";
        };
      };

      networking.wg-quick.interfaces."airvpn-sweden" = {
        address = [
          "10.164.48.170/32"
          "fd7d:76ee:e68f:a993:7929:eccb:f9f:330f/128"
        ];
        privateKeyFile = "/run/secrets/server_airvpn_private";
        mtu = 1320;

        postUp = "${pkgs.iptables}/bin/iptables -I OUTPUT ! -o airvpn-sweden -m mark ! --mark $(wg show airvpn-sweden fwmark) -m addrtype ! --dst-type LOCAL -j REJECT";
        # There is no preDown on purpose, just restart the container if the vpn ever goes down

        peers = [
          {
            presharedKeyFile = "/run/secrets/server_airvpn_preshared";
            publicKey = "PyLCXAQT8KkM4T+dUsOQfn+Ub3pGxfGlxkIApuig+hk=";
            endpoint = "se3.vpn.airdns.org:1637";
            allowedIPs = [
              "0.0.0.0/0"
              "::/0"
            ];
            persistentKeepalive = 15;
          }
        ];
      };
    };
  };
}
