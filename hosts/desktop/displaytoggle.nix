{
  writeShellApplication,
  jq,
  niri,
}:
writeShellApplication {
  name = "displaytoggle";
  runtimeInputs = [
    jq
    niri
  ];

  text = let
    main = "DP-3";
  in ''
    if niri msg -j outputs | jq -e '.[] | select(.name != "${main}") | .logical != null' > /dev/null; then
      echo "on"
      for mon in $(niri msg -j outputs | jq -r '.[] | select(.name != "${main}") | .name'); do
        niri msg output "$mon" off
      done
    else
      for mon in $(niri msg -j outputs | jq -r '.[] | .name'); do
        niri msg output "$mon" on
      done
    fi
  '';
}
