# various shit for school
{
  pkgs,
  config,
  mlib,
  lib,
  ...
}:
let
  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf;

  cfg = config.meow.school;
in
{
  options.meow.school.enable = mkEnOpt "Enable school stuff";

  config = mkIf cfg.enable {
    programs.firejail = {
      enable = true;
      wrappedBinaries = {
        packettracer =
          let
            pkg = pkgs.cisco-packet-tracer_9;
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

    meow.home.modules = [
      {
        mHome.lang.c_sharp = true;
        mHome.lang.python = true;

        home.packages = with pkgs; [
          lmath # math editor
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
  };
}
