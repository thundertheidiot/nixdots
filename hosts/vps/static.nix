{
  config,
  lib,
  ...
}:
let
  inherit (lib)
    head
    mapAttrs'
    nameValuePair
    replaceStrings
    ;

  domain = head config.meow.server.services.static.expose;
  root = "/nix/persist/static";

  # subdir -> sops encrypted htpasswd file
  # e.g. private = ./secrets/static-private.htpasswd;
  protected = {
    "treenaus/ohjelmoitavat" = ./secrets/static-ohjelmoitavat.htpasswd;
  };

  secretName = dir: "static_htpasswd_${replaceStrings [ "/" ] [ "_" ] dir}";
in
{
  config = {
    meow.server.services.static = {
      internal = null;
      expose = [ "static.meowcloud.net" ];
    };

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
      locations =
        mapAttrs' (
          dir: _:
          nameValuePair "^~ /${dir}/" {
            basicAuthFile = config.sops.secrets.${secretName dir}.path;
          }
        ) protected
        // {
          "~ ^/([^/]+/)?\\.".extraConfig = "deny all;";
        };
    };
  };
}
