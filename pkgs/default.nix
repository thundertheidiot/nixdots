{pkgs}:
with pkgs; {
  cru = callPackage ./cru.nix {};
  dgr = callPackage ./dgr.nix {};
  sable = callPackage ./sable.nix {};
  kodi = callPackage ./kodi {};
  bandcamp-dl = callPackage ./bandcamp-dl.nix {};
}
