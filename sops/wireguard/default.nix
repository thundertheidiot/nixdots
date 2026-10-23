let
  inherit (builtins)
    fromJSON
    readFile
    mapAttrs
    attrNames
    filter
    listToAttrs
    ;
  inventory = fromJSON (readFile ./peers.json);
  publicKeys = fromJSON (readFile ./public-keys.json);
in
inventory
// {
  peers = mapAttrs (name: address: {
    inherit address;
    publicKey = publicKeys.${name};
    module = {
      sops.secrets = {
        wg_private = {
          sopsFile = ./. + "/private/${name}";
          format = "binary";
          restartUnits = [ "wg-quick-wg0.service" ];
        };
      }
      // listToAttrs (
        map
          (leaf: {
            name = "wg_psk_${leaf}";
            value = {
              sopsFile = ./. + "/psk/${leaf}";
              format = "binary";
              restartUnits = [ "wg-quick-wg0.service" ];
            };
          })
          (
            if name == inventory.hub then
              filter (peer: peer != inventory.hub) (attrNames inventory.peers)
            else
              [ name ]
          )
      );
    };
  }) inventory.peers;
}
