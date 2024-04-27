let
  modules = [
    (import ./wayland)
    (import ./hyprland.nix)
  ];
in {
  system = { mlib, ... }: {
    imports = mlib.getSystems modules;
  };

  home = { mlib, ... }: {
    imports = mlib.getHomes modules;
  };
}
