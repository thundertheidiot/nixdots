let
  modules = [
    (import ./wayland)
    (import ./hyprland.nix)
    (import ./plasma)
  ];
in {
  system = { mlib, ... }: {
    imports = mlib.getSystems modules;
  };

  home = { mlib, ... }: {
    imports = mlib.getHomes modules;
  };
}
