{
  config,
  lib,
  pkgs,
  mlib,
  ...
}: let
  cfg = config.meow.cfg;
in {
  options = let
    inherit (mlib) mkOpt mkEnOpt;
    inherit (lib.types) listOf attrs;
  in {
    meow.cfg = {
      symlinkFiles = mkOpt (listOf attrs) [] {};
      mountDirectories = mkOpt (listOf attrs) [] {};
    };
  };

  config = let
    inherit (lib.strings) concatStringsSep;
    
    symlinkScript = ''
    ${concatStringsSep "\n" (map (f: with f;
      "ln -sf \"${source}\" \"${destination}\"") cfg.symlinkFiles)}
    '';

    mountScript = ''
    ${concatStringsSep "\n" (map (f: with f; ''
      mkdir -p "${destination}"
      if [ $(mountpoint -q "${destination}" ) ]; then
        mount -o bind,remount,ro "${source}" "${destination}"
      else
        mount -o bind,ro "${source}" "${destination}"
      fi
      '') cfg.mountDirectories)}
    '';
  in {
    system.activationScripts = {
      configurationSymlink.text = symlinkScript;
      configurationMount.text = mountScript;
    };
  };
}
