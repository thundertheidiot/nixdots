{
  flake.modules.nixos.desktop = {pkgs, ...}: {
    programs.command-not-found.enable = true;
    programs.command-not-found.dbPath = "${pkgs.path}/programs.sqlite";
  };
}
