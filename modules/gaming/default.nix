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
        # (pkgs.writeShellScriptBin "newgamescope" ''exec "${pkgs.gamescope}/bin/gamescope" "$@"'')
        # (pkgs."2405".gamescope.overrideAttrs rec {
        #   name = "gamescope-${version}";
        #   version = "3.14.0";

        #   src = pkgs.fetchFromGitHub {
        #     owner = "ValveSoftware";
        #     repo = "gamescope";
        #     rev = "refs/tags/${version}";
        #     fetchSubmodules = true;
        #     hash = "sha256-lgVVhnj209o9kCGTxOGmCRCyhT91QRvlQfOYyvyGj2Y=";
        #   };

        #   postInstall = ''
        #         mv $out/bin/gamescope $out/bin/gamescope-${version}
        #     #   '';
        # })

        (pkgs."2405".gamescope.overrideAttrs rec {
          name = "gamescope-24-05";

          postInstall = ''
            mv $out/bin/gamescope $out/bin/${name}
          '';
        })
      ];

      programs.gamescope = {
        enable = true;
        # The newest gamescope refuses to actually work with any games
        # I don't know why but i can do this
        # package = pkgs."2405".gamescope.overrideAttrs (final: prev: rec {
        #   version = "3.14.0";

        #   # patches = [
        #   #   ./gamescope_shaders-path.patch
        #   #   ./gamescope_use-pkgconfig.patch
        #   # ];

        #   # postPatch = ''
        #   # substituteInPlace src/reshade_effect_manager.cpp --replace "@out@" "$out"
        #   # '';

        #   src = pkgs.fetchFromGitHub {
        #     owner = "ValveSoftware";
        #     repo = "gamescope";
        #     rev = "refs/tags/${version}";
        #     fetchSubmodules = true;
        #     hash = "sha256-lgVVhnj209o9kCGTxOGmCRCyhT91QRvlQfOYyvyGj2Y=";
        #   };
        # });
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
