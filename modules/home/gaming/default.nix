{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  # This moves .steam .steampid and .steampath out of $HOME
  startsteam = pkgs.writeShellScriptBin "startsteam" ''
    HOME="$XDG_DATA_HOME/steamhome" steam $*
  '';
in lib.mkIf (config.setup.gaming.enable) (with config; {
  home.packages = with pkgs;
    [
      lutris
      startsteam
      discord
      webcord

      prismlauncher

      retroarchBare
      retroarch-assets
    ]
    ++ (with pkgs.ataraxiasjel; [
      proton-ge
      wine-ge
    ]);
})
