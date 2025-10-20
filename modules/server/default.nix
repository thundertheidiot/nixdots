{
  config,
  mlib,
  lib,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib) head;
  inherit (lib.types) listOf str;
in {
  options.meow.server = {
    domains = mkOpt (listOf str) [] {};
    mainDomain = mkOpt str (head config.meow.server.domains) {};
  };

  imports = [
    ./acme.nix
    ./coturn.nix
    ./deploy.nix
    ./mail.nix
    ./mumble.nix
    ./prosody
    ./radio.nix
    ./ssh.nix
    ./vaultwarden.nix
    ./webserver.nix
  ];
}
