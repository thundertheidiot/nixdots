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
  inherit (lib.lists) foldl;
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

      meow.sops.secrets."rathole" = {
        sopsFile = ./server/rathole.toml;
        format = "binary";
      };
    }
    {
      sops.secrets =
        filterAttrs (
          name: _: (foldl (bool: elem:
              if bool || name == elem
              then true
              else false)
            false
            config.meow.sops.enableSecrets)
        )
        config.meow.sops.secrets;
    }
  ];
}
