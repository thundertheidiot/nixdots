{
  lib,
  writeShellApplication,
  yq,
  kodiHome,
  kodiSettings,
  mlib,
  ...
}: let
  inherit (builtins) toString;
  inherit (lib) mapAttrsToList;
  inherit (lib.strings) toJSON concatStringsSep;
  inherit (mlib) jqMergeFileWithValue;

  # This is some magic stolen from
  # https://stackoverflow.com/questions/53661930/jq-recursively-merge-objects-and-concatenate-arrays
  # jqDeepmerge = ''
  #   def deepmerge(a;b):
  #   reduce b[] as $item (a;
  #     reduce ($item | keys_unsorted[]) as $key (.;
  #       $item[$key] as $val | ($val | type) as $type | .[$key] = if ($type == "object") then
  #         deepmerge({}; [if .[$key] == null then {} else .[$key] end, $val])
  #       elif ($type == "array") then
  #         (.[$key] + $val | unique)
  #       else
  #         $val
  #       end)
  #   );'';

  createXml =
    mapAttrsToList (
      name: value: let
        file = "${kodiHome}/${name}";
      in
        # ''
        #   if [ ! -f "${file}" ]; then
        #     mkdir -p "$(dirname "${file}")"
        #     echo -e "<settings>\n</settings>" > "${file}"
        #   fi
        #   xq --argjson nix_settings '${toJSON value}' '${jqDeepmerge} deepmerge({}; [., $nix_settings])' --xml-output < "${file}" > "${file}.tmp"
        #   mv "${file}.tmp" "${file}"
        #   echo Applied settings for "${file}"
        # ''
        jqMergeFileWithValue {
          jq = "xq";
          inherit file value;
        }
    )
    kodiSettings;
in
  writeShellApplication {
    name = "create_settings";

    excludeShellChecks = ["SC2016"];

    runtimeInputs = [yq];

    text = concatStringsSep "\n" createXml;
  }
