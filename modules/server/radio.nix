{
  config,
  mlib,
  lib,
  pkgs,
  ...
}: let
  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf;

  cfg = config.meow.server;
in {
  options.meow.server.radio = mkEnOpt "Radio";

  config = mkIf cfg.radio {
    services.icecast = {
      enable = true;
      hostname = "localhost";
      listen.address = "127.0.0.1";
      listen.port = 8002;

      # should only be listening on localhost
      admin.password = "icecast";
    };

    services.liquidsoap.streams = {
      radio = pkgs.writeText "radio.liq" ''
        set("log.stdout", true)

        files = playlist(mode="randomize", "${config.meow.impermanence.persist}/radio")

        output.icecast(
          %vorbis(samplerate=44100, channels=2, quality=0.6),
          host = "127.0.0.1", port = ${toString config.services.icecast.listen.port},
          password = "${config.services.icecast.admin.password}", mount = "radio.ogg",
          mksafe(files)
        )
      '';
    };
  };
}
