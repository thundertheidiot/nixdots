{ config, pkgs, ... }: {
  config.programs.fish = {
    enable = true;
    shellAliases = {
      "m" = "mpv --no-video --loop=yes";
    };
  };
}
