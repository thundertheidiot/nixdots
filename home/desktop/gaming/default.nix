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
    HOME="$XDG_DATA_HOME/steamhome" steam
  '';
in lib.mkIf (config.setup.gaming.enable) (with config; {
  home.packages = with pkgs;
    [
      lutris
      startsteam
      discord
    ]
    ++ (with inputs.nix-gaming.packages.${pkgs.system}; [
      # wine-ge
    ]);
})
