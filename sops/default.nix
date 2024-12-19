{
  config,
  lib,
  mlib,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib.types) listOf str attrsOf anything;
  inherit (lib) mkMerge;
  inherit (lib.attrsets) filterAttrs;
  inherit (lib.lists) elem;
in {
  options = {
    meow.sops = {
      enableSecrets = mkOpt (listOf str) [] {
        description = ''
          Sops secrets to enable. Since all of our secrets are configured simultaniously in one place, we need a way to enable only some of them.
          Otherwise every machine gets the activationScript, and fails on every activation without a key.
        '';
      };
      secrets = mkOpt (attrsOf anything) {} {};
    };
  };

  config = mkMerge [
    {
      meow.sops.secrets."youtube_api_keys" = {
        sopsFile = ./digiboksi/youtube;
        format = "binary";
      };

      meow.sops.secrets."server_rathole" = {
        sopsFile = ./server/rathole.toml;
        format = "binary";
      };

      meow.sops.secrets."server_torrent_stack_env" = {
        sopsFile = ./server/torrent_stack_env;
        format = "binary";
      };

      meow.sops.secrets."server_vaultwarden_env" = {
        sopsFile = ./server/vaultwarden.env;
        format = "dotenv";
      };

      meow.sops.secrets."server_homepage_env" = {
        sopsFile = ./server/homepage.env;
        format = "dotenv";
      };

      meow.sops.secrets."desk_rathole" = {
        sopsFile = ./desk/rathole.toml;
        format = "binary";
      };
    }
    {
      sops.secrets =
        filterAttrs (
          name: _: elem name config.meow.sops.enableSecrets
        )
        config.meow.sops.secrets;
    }
  ];
}
