{
  config,
  lib,
  mlib,
  ...
}: let
  inherit (mlib) mkEnOptTrue;
  inherit (lib) mkIf;

  cfg = config.meow.server;
in {
  # https://github.com/NixOS/nixpkgs/pull/260006
  options.meow.server.prosodyModule = mkEnOptTrue "Prosody module";

  config = mkIf cfg.prosodyModule {
    imports = [./prosody.nix];
    disabledModules = ["services/networking/prosody.nix"];
  };
}
