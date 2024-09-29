{
  config,
  pkgs,
  lib,
  mlib,
  mpkgs,
  ...
}: let
  inherit (mlib) mkOpt mkEnOpt;

  cfg = config.meow.gaming;
in {
  options = {
    meow.gaming = {
      enable = mkEnOpt "Enable gaming module";
      games = mkOpt (lib.types.listOf (lib.types.enum ["duckgame" "minecraft"])) [] {};
      emulation = mkEnOpt "Enable configuration for emulation.";
    };
  };

  imports = [
    ./mangohud.nix
  ];

  config = lib.mkMerge [
    (lib.mkIf (cfg.enable) {
      environment.systemPackages = let
        inherit (pkgs.ataraxiasjel) proton-ge wine-ge;
        inherit (builtins) elem;
      in
        with pkgs; [
          lutris
          mangohud

          wine-ge
          proton-ge

          (lib.mkIf (elem "minecraft" cfg.games) prismlauncher)
          (lib.mkIf (elem "duckgame" cfg.games) (pkgs.callPackage mpkgs.dgr {homeDirectory = config.stubbornHomeDirectory;}))
        ];

      programs.gamemode.enable = true;
      services.joycond.enable = true;
    })
    (lib.mkIf (cfg.enable && cfg.emulation) {
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

      environment.systemPackages = [
        (pkgs."2405".gamescope.overrideAttrs rec {
          name = "gamescope-24-05";

          postInstall = ''
            mv $out/bin/gamescope $out/bin/${name}
          '';
        })
      ];

      programs.gamescope = {
        enable = true;
      };

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
