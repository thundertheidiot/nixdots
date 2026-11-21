{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (builtins) toJSON;
  inherit (lib)
    boolToString
    genAttrs
    getExe
    mapAttrs'
    mkForce
    nameValuePair
    ;
in
{
  # nixflix's host config payload lacks the oidc/trustedNetworks/sslKeyPath fields of
  # newer arr versions
  # TODO fix when nixflix is updated
  systemd.services = mapAttrs' (name: nameValuePair "${name}-config") (
    genAttrs [ "radarr" "sonarr" ] (
      name:
      let
        hc = config.nixflix.${name}.config.hostConfig;
        secrets = "/run/media-stack/secrets";
        curl = "${getExe pkgs.curl} --variable apiKey@${secrets}/${name}-api-key --expand-header 'X-Api-Key: {{apiKey:trim}}' -sf";
        jq = getExe pkgs.jq;
      in
      {
        script = mkForce ''
          set -eu
          url="http://${hc.bindAddress}:${toString hc.port}${hc.urlBase}/api/v3/config/host"
          current=$(${curl} "$url")
          id=$(echo "$current" | ${jq} -r .id)
          body=$(echo "$current" | ${jq} \
            --rawfile apiKey ${secrets}/${name}-api-key \
            --rawfile password ${secrets}/${name}-ui-password \
            '. + {
              bindAddress: ${toJSON hc.bindAddress},
              port: ${toString hc.port},
              authenticationMethod: ${toJSON hc.authenticationMethod},
              authenticationRequired: ${toJSON hc.authenticationRequired},
              logLevel: ${toJSON hc.logLevel},
              branch: ${toJSON hc.branch},
              urlBase: ${toJSON hc.urlBase},
              instanceName: ${toJSON hc.instanceName},
              updateMechanism: ${toJSON hc.updateMechanism},
              analyticsEnabled: ${boolToString hc.analyticsEnabled},
              username: "admin",
              apiKey: ($apiKey | sub("\n+$"; "")),
              password: ($password | sub("\n+$"; "")),
              passwordConfirmation: ($password | sub("\n+$"; ""))
            }')
          echo "$body" | ${curl} -X PUT -H 'Content-Type: application/json' --data-binary @- "$url/$id" > /dev/null
          systemctl restart ${name}.service
        '';
      }
    )
  );
}
