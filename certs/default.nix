{
  module = {config, ...}: {
    sops.secrets.rootCA = {
      sopsFile = ./rootCA.key;
      format = "binary";
    };

    sops.secrets.localKey = {
      sopsFile = ./local.key;
      format = "binary";
      owner = config.services.nginx.user;
    };

    security.pki.certificateFiles = [./rootCA.pem];
  };

  "local.crt" = ./local.crt;
  "local.csr" = ./local.csr;
  "rootCA.pem" = ./rootCA.pem;
  "san.ext" = ./san.ext;
}
