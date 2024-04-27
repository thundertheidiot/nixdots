{ lib, config, pkgs, ... }: {
  config = lib.mkIf (config.setup.gaming.enable) (with config; {
    programs.steam = {
      enable = true;
      # https://github.com/NixOS/nixpkgs/issues/162562#issuecomment-1229444338
      package = pkgs.steam.override {
        extraPkgs = pkgs: with pkgs; [
          libkrb5
          keyutils
        ];
      };
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
    };

    programs.gamescope = {
      enable = true;
      # Mouse movement = spinbot 
      # https://github.com/ValveSoftware/gamescope/issues/1184
      # https://github.com/ValveSoftware/gamescope/issues/1141
      package = pkgs.gamescope.overrideAttrs (final: prev: rec {
        version = "3.14.0";

        src = pkgs.fetchFromGitHub {
          owner = "ValveSoftware";
          repo = "gamescope";
          rev = "refs/tags/${version}";
          fetchSubmodules = true;
          hash = "sha256-lgVVhnj209o9kCGTxOGmCRCyhT91QRvlQfOYyvyGj2Y=";
        };
      });
    };

    programs.nix-ld.libraries = with pkgs; [
      libGL
      gperftools
    ];

    services.flatpak.enable = true;

    programs.gamemode.enable = true;

    services.joycond.enable = true;

    systemd.services."steamvr-setcap" = {
      enable = false;
      description = "Run setcap to fix steamvr.";
      unitConfig.Type = "simple";
      serviceConfig = {
        ExecStart = "${pkgs.libcap}/bin/setcap CAP_SYS_NICE+ep ${config.homeDirectory}/.local/share/Steam/steamapps/common/SteamVR/bin/linux64/vrcompositor-launcher";
      };
      wantedBy = [ "multi-user.target" ];
    };
  });
}
