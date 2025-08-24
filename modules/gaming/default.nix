{
  config,
  pkgs,
  lib,
  mlib,
  ...
}: let
  inherit (mlib) mkOpt mkEnOpt;
  inherit (lib) mkForce;

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
    {
      boot.kernelPackages = mkForce pkgs.linuxPackages_cachyos-lto;
      # chaotic.mesa-git.enable = true;
    }
    (lib.mkIf (cfg.enable) {
      environment.systemPackages = let
        # inherit (pkgs.ataraxiasjel) proton-ge wine-ge;
        inherit (builtins) elem;
      in
        with pkgs; [
          lutris
          mangohud

          # wine-ge
          # proton-ge

          (lib.mkIf (elem "minecraft" cfg.games) prismlauncher)
          (lib.mkIf (elem "duckgame" cfg.games) (mpkgs.dgr))
        ];

      programs.gamemode.enable = true;
      services.joycond.enable = true;
      programs.joycond-cemuhook.enable = true;
    })
    (lib.mkIf (cfg.enable && cfg.emulation) {
      environment.systemPackages = with pkgs; [
        # pkgs."2411".retroarchFull
        # (callPackage retroarch {
        #   cores = with libretro; [
        #     snes9x
        #     bsnes
        #     parallel-n64
        #   ];
        # })
        retroarch-full
        protontricks
      ];
    })
    (lib.mkIf (cfg.enable) {
      programs.steam = {
        enable = true;

        remotePlay.openFirewall = true;
        localNetworkGameTransfers.openFirewall = true;
        extest.enable = true;

        package = pkgs.steam.override {
          extraPkgs = pkgs:
            with pkgs; [
              # apex legends maybe   rip :(
              libkrb5
              keyutils

              # steamvr
              libcef
              openvr
              gperftools # steamvr home symlink libtcmalloc_minimal.so.0

              # far cry mod installer
              xorg.libSM
            ];

          extraLibraries = pkgs:
            with pkgs; [
              gperftools
              xorg.libxcb
            ];

          extraEnv = {
            HOME = "${config.meow.home.stubbornHomeDirectory}"; # send steam to jail
          };
        };
      };

      programs.gamescope = {
        enable = true;
        capSysNice = false;
      };

      programs.gamemode = {
        enable = true;
        settings = {
          gpu = {
            amd_performance_level = "high";
          };
        };
      };

      services.ananicy = {
        enable = true;
        package = pkgs.ananicy-cpp;
        rulesProvider = pkgs.ananicy-rules-cachyos;
        extraRules = [
          {
            name = "GameThread";
            nice = -20;
          }
        ];
      };
    })
  ];
}
