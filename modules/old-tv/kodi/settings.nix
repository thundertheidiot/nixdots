{
  lib,
  writeShellApplication,
  yq,
  kodiHome,
  ...
}: let
  inherit (builtins) toString toJSON;
  inherit (lib) mapAttrsToList;
  inherit (lib.strings) concatStringsSep;

  kodiSettings = let
    mkSetting = id: val: {
      "@id" = id;
      "@default" = "false";
      "#text" = toString val;
    };
  in {
    "userdata/guisettings.xml" = {
      settings = {
        "@version" = "2";
        setting = [
          (mkSetting "lookandfeel.skin" "skin.estuary.modv2")
          (mkSetting "locale.timezonecountry" "Finland")
          (mkSetting "locale.timezone" "Europe/Helsinki")
          (mkSetting "locale.use24hourclock" true)
          (mkSetting "general.addonupdates" 2)
        ];
      };
    };
  };

  # This is some magic stolen from
  # https://stackoverflow.com/questions/53661930/jq-recursively-merge-objects-and-concatenate-arrays
  jqDeepmerge = ''
    def deepmerge(a;b):
    reduce b[] as $item (a;
      reduce ($item | keys_unsorted[]) as $key (.;
        $item[$key] as $val | ($val | type) as $type | .[$key] = if ($type == "object") then
          deepmerge({}; [if .[$key] == null then {} else .[$key] end, $val])
        elif ($type == "array") then
          (.[$key] + $val | unique)
        else
          $val
        end)
      );'';

  createXml =
    mapAttrsToList (name: value: let
      file = "${kodiHome}/${name}";
    in ''
      if [ ! -f "${file}" ]; then
        mkdir -p "$(dirname ${file})"
        echo -e "<settings>\n</settings>" > "${file}"
      fi
      xq --argjson nix_settings '${toJSON value}' '${jqDeepmerge} deepmerge({}; [., $nix_settings])' --xml-output < "${file}" > "${file}.tmp"
      mv "${file}.tmp" "${file}"
    '')
    kodiSettings;
in
  writeShellApplication {
    name = "create_kodi_settings";

    excludeShellChecks = ["SC2016"]; # shellcheck was failing on the single quotes in the xq command

    runtimeInputs = [yq];

    text = ''
      [ ! -d "${kodiHome}/userdata/addon_data/script.skinshortcuts" ] \
      && mkdir -p "${kodiHome}/userdata/addon_data" \
      && cp -r --dereference "${./script.skinshortcuts-settings}" "${kodiHome}/userdata/addon_data/script.skinshortcuts" \
      && chown "$USER" "${kodiHome}/userdata/addon_data/script.skinshortcuts" \
      && chmod -R 755 "${kodiHome}/userdata/addon_data/script.skinshortcuts"

      ${concatStringsSep "\n" createXml}
    '';
  }
