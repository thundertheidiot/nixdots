let
  inherit (builtins) fromJSON readFile mapAttrs;

  publicKeys = fromJSON (readFile ./public-keys.json);

  mkWg = name: pubkey: {
    inherit pubkey;
    module = {
      sops.secrets = {
        wg_private = {
          sopsFile = ./private-keys.json;
          format = "json";
          key = name;
        };

        wg_preshared = {
          sopsFile = ./preshared-key;
          format = "binary";
        };
      };
    };
  };
in
  mapAttrs mkWg publicKeys
