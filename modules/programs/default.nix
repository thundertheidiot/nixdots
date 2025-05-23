# TODO: bad naming/organization?
{
  pkgs,
  config,
  mlib,
  lib,
  ...
}: let
  cfg = config.meow.program;
in {
  options = let
    inherit (mlib) mkEnOpt;
  in {
    meow.program = {
      # creation
      blender = mkEnOpt "Blender";
      obs = mkEnOpt "OBS";
      kdenlive = mkEnOpt "Kdenlive";
      godot = mkEnOpt "Godot";
      gimp = mkEnOpt "Gimp";

      # im
      element = mkEnOpt "Element";
      signal = mkEnOpt "Signal";
      gajim = mkEnOpt "Gajim";
      mumble = mkEnOpt "Mumble";
      discord = mkEnOpt "Discord";

      libreoffice = mkEnOpt "Libreoffice";
      speedcrunch = mkEnOpt "Speedcrunch";

      freetube = mkEnOpt "Youtube client";
      ansel = mkEnOpt "Ansel";
    };
  };

  imports = [
    ./discord
    ./alacritty.nix
    ./sets.nix
  ];

  config = let
    inherit (lib) mkIf;
  in {
    environment.systemPackages = with pkgs; let
    in [
      (mkIf cfg.element element-desktop)
      (mkIf cfg.signal signal-desktop-bin)
      (mkIf cfg.gajim gajim)
      (mkIf cfg.mumble mumble)
      (mkIf cfg.discord vesktop)

      (mkIf cfg.blender pkgs."2411".blender)
      (mkIf cfg.obs obs-studio)
      (mkIf cfg.kdenlive kdePackages.kdenlive)
      (mkIf cfg.godot godot_4)
      (mkIf cfg.gimp gimp)

      (mkIf cfg.libreoffice libreoffice)
      (mkIf cfg.speedcrunch speedcrunch)

      (mkIf cfg.freetube freetube)
      (mkIf cfg.ansel ansel)
    ];
  };
}
