{
  config,
  inputs,
  mlib,
  lib,
  ...
}:
let
  inherit (mlib) mkEnOptTrue;
  inherit (lib) mkIf mkMerge;

  cfg = config.meow.certificates;
in
{
  options = {
    meow.certificates = mkEnOptTrue "Enable self signed certificates for local server.";
  };

  config = mkMerge [
    {
      # compromised root ca
      security.pki.caCertificateBlacklist = [
        "Comodo AAA Services root"
        "emSign ECC Root CA - G3"
      ];
    }
    (mkIf config.meow.certificates {
      security.pki.certificateFiles = [ ../certs/rootCA.pem ];
    })
  ];
}
