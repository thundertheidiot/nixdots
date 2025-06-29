# This module is
{
  config,
  pkgs,
  lib,
  mlib,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib.types) bool str;
  inherit (lib) mkIf;

  en = config.meow.base;
in {
  options = {
    meow.base = mkOpt bool true {
      description = "Base setup for every machine, including servers.";
    };

    meow.timeZone = mkOpt str "Europe/Helsinki" {};
    meow.hostName = mkOpt str "meow" {};
  };

  config = mkIf en {
    nixpkgs.config.allowUnfree = true;

    # nix.package = pkgs.lixPackageSets.latest.lix;

    nix.settings =
      {
        experimental-features = ["nix-command" "flakes"];
        use-xdg-base-directories = true;
        allow-import-from-derivation = true;
      }
      # Is this stupid? Yes, unfortunately flakes are stupid too, and the attributes cannot be computed, but i also want a single source of truth for these
      # https://github.com/NixOS/nix/issues/4945
      # TODO the real question here is if this is ever needed in practice
      // (import ../flake.nix).nixConfig;

    boot.tmp.cleanOnBoot = true;
    hardware.enableRedistributableFirmware = true;

    # doesn't work on lix, doesn't work with flakes anyway?
    system.tools.nixos-option.enable = false;

    networking.networkmanager.enable = true;

    programs.command-not-found.enable = true;
    programs.command-not-found.dbPath = "${pkgs.path}/programs.sqlite";

    systemd.extraConfig = ''
      DefaultTimeoutStopSec=3s
    '';

    security.sudo.enable = lib.mkForce false;
    security.sudo-rs = {
      enable = true;
      execWheelOnly = true;
    };

    i18n.defaultLocale = "en_US.UTF-8";

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
