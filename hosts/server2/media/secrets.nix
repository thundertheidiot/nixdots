{ config, lib, ... }:
let
  inherit (lib) genAttrs mapAttrs recursiveUpdate;
  inherit (builtins)
    attrNames
    fromJSON
    toJSON
    readFile
    ;
  slskd = import ./slskd.nix;
  secret = name: config.sops.placeholder.${name};
  names = attrNames (removeAttrs (fromJSON (readFile ../secrets/media-stack.json)) [ "sops" ]);
in
{
  config = {
    sops.templates."media-stack-homepage" = {
      restartUnits = [ "homepage-dashboard.service" ];
      mode = "0400";
      content = ''
        HOMEPAGE_VAR_QBIT_USERNAME=${secret "radarr-qbit-username"}
        HOMEPAGE_VAR_QBIT_PASSWORD=${secret "radarr-qbit-password"}
        HOMEPAGE_VAR_RADARR_API=${secret "radarr-api-key"}
        HOMEPAGE_VAR_SONARR_API=${secret "sonarr-api-key"}
        HOMEPAGE_VAR_LIDARR_API=${secret "lidarr-api-key"}
        HOMEPAGE_VAR_PROWLARR_API=${secret "prowlarr-api-key"}
      '';
    };
    sops.templates."media-stack-slskd" = {
      restartUnits = [ "slskd.service" ];
      path = "/run/media-stack/slskd.yml";
      mode = "0400";
      content = toJSON (
        recursiveUpdate slskd {
          soulseek = {
            username = secret "slskd-soulseek-username";
            password = secret "slskd-soulseek-password";
          };
          web.authentication = {
            username = secret "slskd-username";
            password = secret "slskd-password";
            api_keys = mapAttrs (name: _: {
              key = secret "slskd-api-${name}";
            }) slskd.web.authentication.api_keys;
          };
        }
      );
    };
    sops.secrets =
      (genAttrs names (name: {
        sopsFile = ../secrets/media-stack.json;
        format = "json";
        key = name;
        path = "/run/media-stack/secrets/${name}";
        mode = "0400";
        owner = "root";
        group = "root";
      }))
      // {
        media-stack-wireguard = {
          sopsFile = ../secrets/airvpn.conf;
          format = "binary";
          key = "";
          path = "/run/media-stack/wireguard.conf";
          mode = "0400";
          owner = "root";
          group = "root";
        };
      };
  };
}
