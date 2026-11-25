# various shit for school
{
  pkgs,
  config,
  mlib,
  lib,
  ...
}:
let
  inherit (mlib) mkEnOpt mkEnOptTrue;
  inherit (lib) mkIf mkMerge;

  cfg = config.meow.school;
in
{
  options.meow.school = {
    enable = mkEnOpt "Enable school stuff";
    packetTracer = mkEnOptTrue "Cisco Packet Tracer";
    lmath = mkEnOptTrue "LMath";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      meow.home.modules = [
        {
          mHome.lang.c_sharp = true;
          mHome.lang.python = true;

          home.packages = with pkgs; [
            (stdenvNoCC.mkDerivation {
              name = "tmc-cli";
              dontUnpack = true;

              nativeBuildInputs = [ makeWrapper ];
              propagatedBuildInputs = [ tmc-cli ];

              # defaults to $HOME/tmc-config lmao
              installPhase = ''
                mkdir -p $out/bin
                makeWrapper ${tmc-cli}/bin/tmc $out/bin/tmc \
                  --set HOME ${config.meow.home.stubbornHomeDirectory}
              '';
            })
          ];
        }
      ];
    }
    (mkIf cfg.lmath {
      meow.home.modules = [
        {
          home.packages = [ pkgs.lmath ]; # math editor
        }
      ];
    })
    (mkIf cfg.packetTracer {
      multiverse.pins.ciscoPacketTracer9 = "9.0.0";

      programs.firejail = {
        enable = true;
        wrappedBinaries = {
          packettracer =
            let
              pkg = config.multiverse.pinned.ciscoPacketTracer9;
            in
            {
              executable = lib.getExe pkg;
              desktop = "${pkg}/share/applications/cisco-packet-tracer-9.desktop";

              extraArgs = [
                "--net=none"
                "--noprofile"
                "--private=${config.meow.home.stubbornHomeDirectory}/packettracer"
                ''--env=QT_STYLE_OVERRIDE=""''
              ];
            };
        };
      };
    })
  ]);
}
