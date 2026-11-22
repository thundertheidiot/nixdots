{
  config,
  mlib,
  lib,
  ...
}:
let
  inherit (mlib) mkOpt;
  inherit (lib) head mkForce mkIf;
  inherit (lib.types) listOf str;
in
{
  options.meow.server = {
    domains = mkOpt (listOf str) [ ] { };
    mainDomain = mkOpt str (head config.meow.server.domains) { };
  };

  imports = [
    # ./mail.nix
    ./acme.nix
    ./coturn.nix
    ./forgejo.nix
    ./jellyfin-proxy.nix
    ./matrix
    ./mumble.nix
    ./prosody
    ./radio.nix
    ./ssh.nix
    ./vaultwarden.nix
    ./webserver.nix
  ];

  config = {
    nix.registry = mkIf (!config.meow.workstation.enable) (mkForce { });
  };
}
