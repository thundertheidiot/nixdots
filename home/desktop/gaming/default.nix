{ config, lib, pkgs, localconfig, inputs, ... }: with config; lib.mkIf (localconfig.install.gaming) {
  home.packages = with pkgs; [
    lutris
  ] ++ (with inputs.nix-gaming.packages.${pkgs.system}; [
    wine-ge
  ]);
}
