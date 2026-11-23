{
  config,
  infra,
  lib,
  mlib,
  ...
}:
let
  inherit (mlib) mkOpt;
  inherit (lib.types)
    listOf
    str
    attrsOf
    anything
    ;
  inherit (lib) mkMerge;
  inherit (lib.attrsets) filterAttrs;
  inherit (lib.lists) elem;

  kanidmSecret = key: owner: {
    sopsFile = infra.secrets.kanidm;
    inherit key owner;
  };
in
{
  options = {
    meow.sops = {
      enableSecrets = mkOpt (listOf str) [ ] {
        description = ''
          Sops secrets to enable. Since all of our secrets are configured simultaniously in one place, we need a way to enable only some of them.
          Otherwise every machine gets the activationScript, and fails on every activation without a key.
        '';
      };
      secrets = mkOpt (attrsOf anything) { } { };
    };
  };

  config = mkMerge [
    {
      meow.sops.secrets."youtube_api_keys" = {
        sopsFile = ./digiboksi/youtube;
        format = "binary";
      };

      meow.sops.secrets."listenbrainz_token" = {
        sopsFile = ./shared/listenbrainz;
        format = "binary";
        owner = config.meow.user;
      };

      meow.sops.secrets."navidrome_password" = {
        sopsFile = ./shared/navidrome-password;
        format = "binary";
        owner = config.meow.user;
      };

      meow.sops.secrets."listenbrainz_username" = {
        sopsFile = ./shared/listenbrainz-username;
        format = "binary";
        owner = config.meow.user;
      };

      # kanidm reads these for provisioning, the apps get them through credentials or root
      meow.sops.secrets."kanidm_forgejo" = kanidmSecret "forgejo" "kanidm";
      meow.sops.secrets."kanidm_jellyfin" = kanidmSecret "jellyfin" "kanidm";
      meow.sops.secrets."kanidm_oauth2_proxy" = kanidmSecret "oauth2-proxy" "kanidm";
      meow.sops.secrets."oauth2_proxy_cookie" = kanidmSecret "oauth2-proxy-cookie" "root";
    }
    {
      sops.secrets = filterAttrs (
        name: _: elem name config.meow.sops.enableSecrets
      ) config.meow.sops.secrets;
    }
  ];
}
