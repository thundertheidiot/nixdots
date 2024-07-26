{
  config,
  pkgs,
  lib,
  mlib,
  ...
}: let
  inherit (mlib) mkOpt mkEnOpt;

  cfg = config.m.gaming;
in {
  options = {
    m.gaming = {
      enable = mkEnOpt "Enable gaming module";
      install = {
        duckgame = mkEnOpt "Duck Game Rebuilt";
      };
      retro = mkEnOpt "Enable configuration for emulation.";
    };
  };

  config = lib.mkMerge [
    (lib.mkIf (cfg.enable) {
      environment.systemPackages = let
        inherit (pkgs.ataraxiasjel) proton-ge wine-ge;
      in with pkgs; [
        lutris
        mangohud
        prismlauncher

        wine-ge
        proton-ge

        (lib.mkIf cfg.install.duckgame (pkgs.callPackage mpkgs.dgr {homeDirectory = config.stubbornHomeDirectory;}))
      ];
    })
    (lib.mkIf (cfg.retro) {
      environment.systemPackages = with pkgs; [
        (retroarch.override {
          cores = with libretro; [
            snes9x
            bsnes
            parallel-n64
          ];
        })
      ];
    })
    (lib.mkIf (cfg.enable) {
      programs.steam = {
        enable = true;

        package = pkgs.steam.override {
          extraPkgs = pkgs:
            with pkgs; [
              # apex legends maybe
              libkrb5
              keyutils

              # steamvr
              libcef
              openvr
              gperftools # steamvr home symlink libtcmalloc_minimal.so.0
            ];

          extraLibraries = pkgs:
            with pkgs; [
              gperftools
            ];

          extraEnv = {
            HOME = config.stubbornHomeDirectory; # send steam to jail
          };
        };
      };

      programs.gamescope = {
        enable = true;
        # The newest gamescope refuses to actually work with any games
        # I don't know why but i can do this
        package = pkgs."2405".gamescope.overrideAttrs (final: prev: rec {
          version = "3.14.0";

          # patches = [
          #   ./gamescope_shaders-path.patch
          #   ./gamescope_use-pkgconfig.patch
          # ];

          # postPatch = ''
          # substituteInPlace src/reshade_effect_manager.cpp --replace "@out@" "$out"
          # '';

          src = pkgs.fetchFromGitHub {
            owner = "ValveSoftware";
            repo = "gamescope";
            rev = "refs/tags/${version}";
            fetchSubmodules = true;
            hash = "sha256-lgVVhnj209o9kCGTxOGmCRCyhT91QRvlQfOYyvyGj2Y=";
          };
        });
      };

      programs.gamemode.enable = true;

      services.joycond.enable = true;

      systemd.services."steamvr-setcap" = {
        enable = false;
        description = "Run setcap to fix steamvr.";
        unitConfig.Type = "simple";
        serviceConfig = {
          ExecStart = "${pkgs.libcap}/bin/setcap CAP_SYS_NICE+ep ${config.homeDirectory}/.local/share/Steam/steamapps/common/SteamVR/bin/linux64/vrcompositor-launcher || true";
        };
        wantedBy = ["multi-user.target"];
      };
    })
  ];
}
