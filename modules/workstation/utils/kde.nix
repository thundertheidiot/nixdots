{
  system = {...}: {};

  home = {
    config,
    pkgs,
    lib,
    ...
  }:
    lib.mkIf (config.workstation.utils == "kde") {
      home.packages = with pkgs; [
        mpdevil
      ];
    };
}
