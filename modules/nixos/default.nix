{
  lib,
  config,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    (import ../workstation).system
    # ../gaming
    # ./tv
    # ./gpu
    # ./desktop.nix
  ];

  config = {
    nix.settings = {
      experimental-features = ["nix-command" "flakes"];
      allowed-users = [config.username];
      use-xdg-base-directories = true;
    };

    boot.tmp.cleanOnBoot = true;

    systemd.extraConfig = ''
      DefaultTimeoutStopSec=3s
    '';

    programs.nix-ld = {
      enable = true;
      libraries = with pkgs; [libGL];
    };

    security.sudo.enable = lib.mkForce false;
    security.sudo-rs = {
      enable = true;
      execWheelOnly = true;
      # extraConfig = ''
      #   Defaults lecture="never"
      # '';
    };

    hardware.enableRedistributableFirmware = true;

    i18n.defaultLocale = "en_US.UTF-8";
    console = {
      useXkbConfig = true;
    };

    networking.networkmanager.enable = true;

    services.xserver = {
      xkb.layout = "us";
      xkb.options = "eurosign:e";
    };
    services.libinput.enable = true;

    environment.systemPackages = with pkgs; [
      neovim
      wget
      git
      clang
      gcc
      cryptsetup

      nh
    ];

    users.users = {
      ${config.username} = {
        extraGroups = ["wheel" "networkmanager" "docker"];
        isNormalUser = true;
      };
    };
  };
}
