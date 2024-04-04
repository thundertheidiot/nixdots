{ lib, config, pkgs, ... }: {
  config = lib.mkIf (config.setup.gaming.enable) (with config; {
    programs.steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
    };

    environment.systemPackages = with pkgs."2311"; [
      gamescope
    ];

    programs.nix-ld.libraries = with pkgs; [
      libGL
      gperftools
    ];

    services.flatpak.enable = true;

    programs.gamemode.enable = true;
  });
}
