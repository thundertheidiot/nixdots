{
  systemArch = "x86_64-linux";

  options = {
    config,
    pkgs,
    ...
  }: {
    username = "iso";
    hostName = "nixos-install";
    timeZone = "Europe/Helsinki";
  };

  home = {...}: {
    home.stateVersion = "24.05";
  };

  system = {
    lib,
    config,
    ...
  }: {
    system.stateVersion = "24.05";
  };
}
