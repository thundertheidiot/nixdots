{
  config,
  lib,
  mlib,
  ...
}: let
  en = config.meow.fullSetup;
in {
  options = {
    meow.fullSetup = mlib.mkEnOpt "Enable all the bells and whistles.";
    meow.baseSetup = mlib.mkEnOpt "Enable commonly needed things, but leave out some heavier ones.";
  };

  config = let
    inherit (lib) listToAttrs mkIf mkMerge;
    enAll = list:
      listToAttrs (map (i: {
          name = i;
          value = true;
        })
        list);
  in
    mkMerge [
      (mkIf config.meow.fullSetup {
        meow.program = enAll [
          "element"
          "signal"
          "gajim"
          "mumble"
          "discord"

          "libreoffice"
          "speedcrunch"

          "blender"
          "obs"
          "kdenlive"
          # "godot"
          "gimp"

          # "freetube"
          # "ansel"
        ];

        home-manager.sharedModules = [
          {
            # keeping these in sync will prove to be a headache
            mHome.lang = enAll [
              "nix"
              "haskell"
              "rust"
              "lua"
              "c_cxx"
              "python"
              "bash"
              "web"
              "latex"
            ];
          }
        ];
      })
      (mkIf config.meow.baseSetup {
        meow.program = enAll [
          "gajim"
          "discord"
          "gimp"
        ];

        home-manager.sharedModules = [
          {
            mHome.lang = enAll [
              "nix"
              "rust"
              "bash"
            ];
          }
        ];
      })
    ];
}
