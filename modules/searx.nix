{
  config,
  lib,
  pkgs,
  mlib,
  ...
}: let
  cfg = config.meow.searx.enable;

  inherit (lib) mkIf;

  inherit (mlib) mkEnOpt;
in {
  options = {
    meow.searx.enable = mkEnOpt "Enable searx";
  };

  config = mkIf cfg {
    services.searx = {
      enable = true;
      settings = {
        use_default_settings = true;

        server.port = 8080;
        server.bind_address = "127.0.0.1";
        server.secret_key = "unimportant_because_this_is_local_lol";

        search.formats = ["html" "json"];
        server.public_instance = false;
      };
    };
  };
}
