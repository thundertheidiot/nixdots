{
  mlib,
  config,
  ...
}: let
  inherit (mlib) homeModule;
in {
  config = homeModule {
    programs.tofi = {
      settings = {
        border-width = 1;
        outline-width = 0;
        corner-radius = 7;
        prompt-text = "run: ";
      };
    };

    services.mako = {
      settings = {
        margin = "10";
        padding = "5";
        border-size = 2;
        border-radius = 6;
        icons = true;
        max-icon-size = 32;
        default-timeout = 2000;
        ignore-timeout = true;
      };
    };
  };
}
