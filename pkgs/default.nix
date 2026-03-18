{pkgs}:
with pkgs; {
  cru = callPackage ./cru.nix {};
  dgr = callPackage ./dgr.nix {};
  sable = callPackage ./sable.nix {};
  kodi = callPackage ./kodi {};
  screenshot = callPackage ./screenshot {};
  wl_screenshot = callPackage ./wl_screenshot.nix {};
}
