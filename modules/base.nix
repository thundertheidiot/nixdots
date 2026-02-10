{
  config,
  pkgs,
  lib,
  mlib,
  inputs,
  ...
}: let
  inherit (mlib) mkOpt;

  inherit (lib.types) bool str;
  inherit (lib) mkIf mkDefault;

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

    boot.initrd.systemd.enable = true;
    systemd.settings.Manager = {
      DefaultTimeoutStopSec = "3s";
    };

    # i'm too brainrotted, i'll reach for a search engine first
    documentation.nixos.enable = false;

    security.sudo.enable = lib.mkForce false;
    security.sudo-rs.enable = true;

    services.speechd.enable = false;

    # doesn't work on lix, doesn't work with flakes anyway?
    system.tools.nixos-option.enable = false;

    nix.package = pkgs.nixVersions.latest;

    nix.settings =
      {
        experimental-features = ["nix-command" "flakes"];
        use-xdg-base-directories = true;
        allow-import-from-derivation = true;
      }
      # Is this stupid? Yes, unfortunately flakes are stupid too, and the attributes cannot be computed, but i also want a single source of truth for these
      # https://github.com/NixOS/nix/issues/4945
      # TODO the real question here is if this is ever needed in practice
      // (import "${inputs.self.outPath}/flake.nix").nixConfig;

    nixpkgs.config = {
      allowUnfree = true;

      # TODO check this
      permittedInsecurePackages = [
        "libsoup-2.74.3"
        "mbedtls-2.28.10" # TODO insecure check why
        "cisco-packet-tracer-8.2.2"
        "ciscoPacketTracer8-8.2.2"
        "python3.13-ecdsa-0.19.1"
      ];
    };

    # nh provides store cleanup, so it is good to have here even on servers
    programs.nh = {
      enable = true;

      clean = {
        enable = true;
        extraArgs = "--keep 5 --keep-since 6d";
        dates = "daily";
      };
    };

    boot.tmp.cleanOnBoot = mkDefault true;
    hardware.enableRedistributableFirmware = mkDefault true;
  };
}
