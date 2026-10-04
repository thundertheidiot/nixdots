{
  callPackage,
  kodi-wayland,
}:
let
  addons = callPackage ./plugins.nix { kodi = kodi-wayland; };
  kodi = kodi-wayland.withPackages (_: addons);
in
kodi.overrideAttrs (prev: {
  name = "kodi_with_addons";
  postBuild = prev.postBuild + ''
    ln -s kodi "$out/bin/kodi_with_addons"
    ln -s share/kodi/addons "$out/addons"
  '';
  passthru = (prev.passthru or { }) // {
    inherit addons;
    inherit (kodi-wayland) pythonPackages;
  };
})
