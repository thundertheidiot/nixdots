{ lib, pkgs }:
let
  inherit (lib) getExe getExe';
in
{
  id = "thunder/qbittorrent";
  description = "Show qBittorrent transfer speeds and toggle limit";
  widget = [ ./transfer.luau ];
  postRun = ''
    substituteInPlace "$out/transfer.luau" \
    --replace-fail @CURL@ "${getExe pkgs.curl} --cacert ${../../../../certs/rootCA.pem}" \
    --replace-fail @XDG_OPEN@ "${getExe' pkgs.xdg-utils "xdg-open"}"
  '';
}
