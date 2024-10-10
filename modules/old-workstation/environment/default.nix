let
  modules = [
    (import ./cosmic.nix)
    # (import ./xdg-desktop-portal.nix)
  ];
in {
  system = {mlib, ...}: {
    imports = mlib.getSystems modules;
  };

  home = {mlib, ...}: {
    imports = mlib.getHomes modules;
  };
}
