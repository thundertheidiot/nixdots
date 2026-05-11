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
      {path = "/var/lib/botamusique";}
    ];

    users.users."murmur".extraGroups = ["acme"];

    services.murmur = {
      enable = true;
      openFirewall = true;

      tls.useACMEHost = cfg.mainDomain;
      bandwidth = 96000;
    };

    services.botamusique = {
      enable = false;
      package = pkgs.botamusique.overrideAttrs (prev: {
        pythonPath = prev.pythonPath ++ [pkgs.python313Packages.audioop-lts];
      });
      settings = {
        bot.music_folder = "music/";
        bot.admin = "thunder";

        commands.play = "p, play";
        commands.yt_search = "ys";
      };
    };
  };
}
