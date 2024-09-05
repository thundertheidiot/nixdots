{
  config,
  lib,
  pkgs,
  mpkgs,
  ...
}:
lib.mkIf (config.setup.gaming.enable) (with config; {
  home.packages = with pkgs; [
    vesktop
    webcord # fallback
  ];
})
