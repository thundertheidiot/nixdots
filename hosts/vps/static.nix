{ config, lib, ... }:
let
  inherit (lib) mapAttrs' nameValuePair replaceStrings;

  domain = "static.meowcloud.net";
  root = "/nix/persist/static";

  # subdir -> sops encrypted htpasswd file
  # e.g. private = ./secrets/static-private.htpasswd;
  protected = {
    "treenaus/ohjelmoitavat" = ./secrets/static-ohjelmoitavat.htpasswd;
  };

  secretName = dir: "static_htpasswd_${replaceStrings [ "/" ] [ "_" ] dir}";
in
{
  meow.server.certificates = [ domain ];

  systemd.tmpfiles.rules = [ "d ${root} 0755 root root -" ];

  sops.secrets = mapAttrs' (
    dir: sopsFile:
    nameValuePair (secretName dir) {
      inherit sopsFile;
      format = "binary";
      owner = config.services.nginx.user;
    }
  ) protected;

  services.nginx.virtualHosts.${domain} = {
    inherit root;
    locations = mapAttrs' (
      dir: _:
      nameValuePair "^~ /${dir}/" {
        basicAuthFile = config.sops.secrets.${secretName dir}.path;
      }
    ) protected
    // {
      "~ ^/\\.".extraConfig = "deny all;";
    };
  };
}
