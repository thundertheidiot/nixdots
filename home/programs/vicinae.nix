{
  config,
  inputs,
  pkgs,
  ...
}:
let
  flavor = config.catppuccin.flavor;
in
{
  programs.vicinae = {
    package = inputs.vicinae.packages.${pkgs.stdenv.hostPlatform.system}.default.override {
      # Vicinae uses GCC 15, dependency must use the same version
      # TODO take out when vicinae updates
      numen = inputs.vicinae.inputs.numen.packages.${pkgs.stdenv.hostPlatform.system}.numen.override {
        stdenv = pkgs.gcc15Stdenv;
        withRepl = false;
      };
    };

    systemd = {
      enable = true;
      autoStart = true;
      environment.USE_LAYER_SHELL = 1;
    };

    settings = {
      font.size = 12;
      window = {
        csd = true;
      };
      theme.dark = {
        name = "catppuccin-${flavor}";
        icon_theme = "default";
      };
    };
  };
}
