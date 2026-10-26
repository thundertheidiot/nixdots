{ pkgs, ... }:
pkgs.writeShellApplication {
  name = "local-certs";
  passthru.autoUpdate.skip = "Local script; dependencies follow flake.lock.";
  runtimeInputs = with pkgs; [
    bash
    coreutils
    diffutils
    git
    jq
    nix
    openssl
    sops
  ];
  text = ''
    exec bash ${../certs/create.sh} "$@"
  '';
}
