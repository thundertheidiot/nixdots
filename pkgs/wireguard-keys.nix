{ pkgs, ... }:
let
  # Work around the pinned registry wrapper and its malformed output translations.
  lisp = (pkgs.sbcl.withPackages (ps: with ps; [ shasht ])).overrideAttrs (old: {
    nativeBuildInputs = [ pkgs.makeWrapper ];
    installPhase = pkgs.lib.replaceStrings
      [ ''--prefix ASDF_OUTPUT_TRANSLATIONS : "$(echo $CL_SOURCE_REGISTRY | sed s,//:,::,g):"'' ]
      [ ''--set ASDF_OUTPUT_TRANSLATIONS '(:output-translations (t t) :ignore-inherited-configuration)' '' ]
      old.installPhase;
  });
in
pkgs.writeShellApplication {
  name = "wireguard-keys";
  runtimeInputs = with pkgs; [
    lisp
    wireguard-tools
    sops
    coreutils
  ];
  text = ''
    exec sbcl --script ${../sops/wireguard/genkeys.lisp} "$@"
  '';
}
