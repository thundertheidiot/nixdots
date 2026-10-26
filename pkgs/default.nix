{ pkgs }:
with pkgs;
{
  cru = callPackage ./cru.nix { };
  dgr = callPackage ./dgr.nix { };
  sable = callPackage ./sable.nix { };
  sable-desktop = callPackage ./sable-desktop.nix { };
  helium = callPackage ./helium.nix { };
  glide = callPackage ./glide.nix { };
  photocraft = callPackage ./photocraft.nix { };
  kodi = callPackage ./kodi { };
  wireguard-keys = callPackage ./wireguard-keys.nix { };
  local-certs = callPackage ./local-certs.nix { };
  firefox-devtools-mcp = callPackage ./firefox-devtools-mcp.nix { };
}
