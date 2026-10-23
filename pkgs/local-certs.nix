{ pkgs, ... }:
pkgs.writeShellApplication {
  name = "local-certs";
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
