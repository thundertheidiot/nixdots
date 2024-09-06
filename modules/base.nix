# This module is
{
  config,
  pkgs,
  lib,
  mlib,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib.types) bool;
  inherit (lib) mkIf;

  en = config.meow.base;
in {
  options = {
    meow.base = mkOpt bool true {
      description = "Base setup for every machine, including servers.";
    };
  };

  config = mkIf en {
    nix.settings = {
      experimental-features = ["nix-command" "flakes"];
      allowed-users = [config.username];
      require-sigs = false;
      use-xdg-base-directories = true;
    };

    boot.tmp.cleanOnBoot = true;
    hardware.enableRedistributableFirmware = true;

    systemd.extraConfig = ''
      DefaultTimeoutStopSec=3s
    '';

    security.sudo.enable = lib.mkForce false;
    security.sudo-rs = {
      enable = true;
      execWheelOnly = true;
    };

    # TODO: useless?
    programs.nix-ld = {
      enable = true;
      libraries = with pkgs; [libGL];
    };

    i18n.defaultLocale = "en_US.UTF-8";

    # TODO: move to workstation?
    users.users = {
      ${config.username} = {
        extraGroups = ["wheel" "networkmanager" "docker"];
        isNormalUser = true;
      };
    };

    environment.systemPackages = with pkgs; [
      git
      wget
      curl

      fd
      ripgrep
      ncdu
      killall
      which

      btop
      neovim
    ];
  };
}
