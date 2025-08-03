{
  writeShellApplication,
  kodi-wayland,
  kodiHome,
  kodiAddons,
  lib,
  sqlite,
  ...
}: let
  inherit (lib) concatStringsSep;
in
  writeShellApplication {
    name = "enable_addons";

    runtimeInputs = [
      sqlite
    ];

    text = ''
      sqlite3 "${kodiHome}/userdata/Database/Addons33.db" \
        "${concatStringsSep " " (map (n: ''UPDATE installed SET enabled=1 WHERE addonID='${n}';'') kodiAddons)}"
    '';
  }
