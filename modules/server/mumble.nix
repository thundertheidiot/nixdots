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

    users.users."murmur".extraGroups = ["acme" "turnserver"];

    services.murmur = {
      enable = true;
      openFirewall = true;

      sslKey = "/var/lib/acme/${head cfg.certificates}/key.pem";
      sslCert = "/var/lib/acme/${head cfg.certificates}/fullchain.pem";
    };

    services.botamusique = {
      enable = true;
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
