{
  config,
  pkgs,
  inputs,
  lib,
  localconfig,
  ...
}: let
  awesome = inputs.nixpkgs-f2k.packages.${pkgs.system}.awesome-git;
in {
  config = lib.mkIf (localconfig.install.awesomewm) (with config; {
    xsession.windowManager.awesome = {
      enable = true;
      package = awesome;
    };

    home.packages = with pkgs; [
      xorg.xinit
    ];

    xdg.configFile."xinitrc" = {
      enable = true;
      text = ''
        #!${pkgs.bash}/bin/bash

        prefix=""
        ${pkgs.which}/bin/which nixGL && prefix="nixGL"

        exec $prefix ${awesome}/bin/awesome > "${xdg.cacheHome}/awesome.log"
      '';
    };

    xdg.configFile."awesome" = {
      enable = true;
      source = ./config;
      recursive = true;
    };
  });
}
