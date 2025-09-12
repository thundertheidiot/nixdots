{...}: let
  inherit (builtins) toJSON;
in rec {
  # This is some magic stolen from
  # https://stackoverflow.com/questions/53661930/jq-recursively-merge-objects-and-concatenate-arrays
  defineJqDeepmerge = ''
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

  applyWithJq = {
    jq ? "jq", # for example yq, tomlq or xq can be used here instead, to work on yaml, toml or xml files
    args ? "",
    operation,
    file,
    outfile ? file,
  }: ''
    ${jq} ${args} '${operation}' < "${file}" > "${outfile}.tmp"
    mv "${outfile}.tmp" "${outfile}"
  '';

  jqMergeFileWithValue = {
    jq ? "jq",
    value,
    file,
    outfile ? file,
    defaultContent ? "{}",
  }: ''
    if [ ! -f "$(dirname "${file}")" ]; then
      mkdir -p "$(dirname "${file}")"
      echo -e "${defaultContent}" > "${file}"
    fi

    ${applyWithJq {
      inherit jq file outfile;
      args = "--argjson value '${toJSON value}'";
      operation = "${defineJqDeepmerge} deepmerge({}; [., $value])";
    }}
  '';
}
