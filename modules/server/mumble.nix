{
  config,
  mlib,
  lib,
  pkgs,
  ...
}: let
  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf head;

  cfg = config.meow.server;
in {
  options.meow.server.mumble = mkEnOpt "Mumble";

  config = mkIf cfg.mumble {
    meow.impermanence.directories = [
      {path = config.services.murmur.stateDir;}
      # {path = "/var/lib/botamusique";}
    ];

    users.users."murmur".extraGroups = ["acme" "turnserver"];

    services.murmur = {
      enable = true;
      openFirewall = true;

      sslKey = "/var/lib/acme/${head cfg.certificates}/key.pem";
      sslCert = "/var/lib/acme/${head cfg.certificates}/fullchain.pem";
    };

    services.botamusique = {
      enable = false;
      package = let
        "2311" = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-23.11.tar.gz") {
          inherit (pkgs) config;
        };
      in
        "2311".botamusique.override {
        };
      # package = pkgs.botamusique.overrideAttrs (prev: {
      #   pythonPath = prev.pythonPath ++ [pkgs.python313Packages.audioop-lts];
      # });
      settings = {
        commands.play = "p, play";
        commands.yt_search = "ys";
      };
    };
  };
}
