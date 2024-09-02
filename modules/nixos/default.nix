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

    users.users = let
      key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBKwHM/9spQfyeNIl/p8N8XBuoKj8UrhuhhlbEwkrgjZ thunder@disroot.org";
    in {
      root.openssh.authorizedKeys.keys = [key];
      ${config.username} = {
        openssh.authorizedKeys.keys = [key];
        extraGroups = ["wheel" "networkmanager" "docker"];
        isNormalUser = true;
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
