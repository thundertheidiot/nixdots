{ config, lib, pkgs, localconfig, inputs, ... }: lib.mkIf (localconfig.install.gaming) (with config; {
  home.packages = with pkgs; [
    lutris
  ] ++ (with inputs.nix-gaming.packages.${pkgs.system}; [
    wine-ge
  ]);
})
