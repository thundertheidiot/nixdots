{
  config,
  mlib,
  lib,
  pkgs,
  ...
}: let
  inherit (mlib) mkEnOpt mkOpt;
  inherit (lib) mkIf listToAttrs;
  inherit (lib.types) listOf str;

  cfg = config.meow.server.radio;
in {
  options.meow.server.radio.enable = mkEnOpt "Radio";
  options.meow.server.radio.domains = mkOpt (listOf str) [config.meow.server.mainDomain] {};

  config = mkIf cfg.enable {
    services.nginx.virtualHosts = listToAttrs (map (name: {
        inherit name;
        value = {
          locations."/radio.ogg" = {
            proxyPass = "http://127.0.0.1:${toString config.services.icecast.listen.port}";
            recommendedProxySettings = true;
          };

          locations."/status-json.xsl" = {
            proxyPass = "http://127.0.0.1:${toString config.services.icecast.listen.port}";
            recommendedProxySettings = true;
          };
        };
      })
      cfg.domains);

    services.icecast = {
      enable = true;
      hostname = "localhost";
      listen.address = "127.0.0.1";
      listen.port = 8000;

      # should only be listening on localhost
      admin.password = "icecast";

      extraConfig = ''
        <authentication>
          <source-password>icecast</source-password>
          <relay-password>icecast</relay-password>
        </authentication>
      '';
    };

    systemd.services.radio.wants = ["network-online.target"];

    services.liquidsoap.streams = {
      radio = pkgs.writeText "radio.liq" ''
        set("log.stdout", true)

        files = playlist(mode="randomize", "${config.meow.impermanence.persist}/radio")

        def update_metadata(m) =
          filename = path.remove_extension(path.basename(m["filename"]))
          [("title", "#{filename}")]
        end

        files = metadata.map(update_metadata, files)

        output.icecast(
          %vorbis(samplerate=44100, channels=2, quality=0.6),
          host = "127.0.0.1", port = ${toString config.services.icecast.listen.port},
          password = "icecast", mount = "radio.ogg",
          mksafe(files)
        )
      '';
    };
  };
}
