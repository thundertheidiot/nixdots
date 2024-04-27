{
  lib,
  config,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    # ./workstation
    (import ../workstation).system
    ./gaming
    ./tv
    ./gpu
    ./laptop
    ./desktop.nix
  ];

  config = with config; {
    nix.settings = {
      experimental-features = ["nix-command" "flakes"];
      allowed-users = [config.username];
    };

    systemd.extraConfig = ''
      DefaultTimeoutStopSec=3s
    '';

    programs.nix-ld = {
      enable = true;
      libraries = with pkgs; [ libGL ];
    };

    security.sudo.enable = true;

    hardware.enableRedistributableFirmware = true;

    i18n.defaultLocale = "en_US.UTF-8";
    console = {
      useXkbConfig = true;
    };

    networking.networkmanager.enable = true;

    services.xserver = {
      xkb.layout = "us";
      xkb.options = "eurosign:e";
      libinput.enable = true;
    };

    users.users.${config.username} = {
      extraGroups = ["wheel" "networkmanager" "docker"];
      isNormalUser = true;
    };

    environment.systemPackages = with pkgs; [
      neovim
      wget
      git
      keyd
      clang
      gcc
      cryptsetup
    ];

    services.keyd = {
      enable = true;
      keyboards.default.ids = [
        "*"
      ];
      keyboards.default.settings = {
        main = {
          capslock = "overload(meta, esc)";
        };
      };
    };

    services.openssh = {
      enable = true;
      settings = {
        PermitRootLogin = "no";
        PasswordAuthentication = false;
      };
    };
  };
}
