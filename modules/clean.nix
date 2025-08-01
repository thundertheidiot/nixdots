{
  flake.modules.nixos.clean = {
    programs.nh.clean = {
      enable = true;
      extraArgs = "--keep 5 --keep-since 6d";
      dates = "daily";
    };
  };
}
