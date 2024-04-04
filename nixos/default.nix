{
  lib,
  config,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ./workstation
    ./gaming
    ./tv
    ./gpu
  ];

  config = with config; {
    nix.settings.experimental-features = ["nix-command" "flakes"];
    nix.settings.allowed-users = [config.username];

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
      extraGroups = ["wheel"];
      isNormalUser = true;
    };

    environment.systemPackages = with pkgs; [
      neovim
      wget
      git
      keyd
      clang
      gcc
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
