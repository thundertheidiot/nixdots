{
  config,
  pkgs,
  inputs,
  lib,
  localconfig,
  ...
}: {
  config = lib.mkIf (localconfig.install.awesomewm) (with config; {
    xsession.windowManager.awesome.enable = true;
    xsession.windowManager.awesome.package = inputs.nixpkgs-f2k.packages.${pkgs.system}.awesome-git;

    home.file.".config/awesome" = {
      source = ./config;
      recursive = true;
    };
  });
}
