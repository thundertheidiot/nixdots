{
  config,
  mlib,
  lib,
  ...
}: let
  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf;

  cfg = config.meow.server;
in {
  options.meow.server.publicSSH = mkEnOpt "Public facing ssh server";

  config = mkIf cfg.publicSSH {
    services.openssh.ports = [69];

    services.endlessh-go = {
      enable = true;
      port = 22;
      listenAddress = "[::]";
      openFirewall = true;
    };
  };
}
