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
  };

  config = lib.mkIf en {
    meow.emacs.lang = {
      latex = true;
      haskell = true;
      fennel = true;
      c_cxx = true;
      bash = true;
      python = true;
    };
  };
}
