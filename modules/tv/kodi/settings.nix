{
  config,
  lib,
  pkgs,
  ...
}: let
  data = {
    "userdata/guisettings.xml" = {
      "/settings/@version" = 2;
      "/settings/setting[@id='lookandfeel.skin']" = "skin.estuary.modv2";
      "/settings/setting[@id='locale.timezonecountry']" = "Finland";
      "/settings/setting[@id='locale.timezone']" = "Europe/Helsinki";
      "/settings/setting[@id='locale.use24hourclock']" = true;
    };
  };

  ensureDir = file: "( mkdir -p \"$(dirname ${file})\"; touch ${file} )";
  xml = "${pkgs.xmlstarlet}/bin/xmlstarlet";

  commandList =
    lib.mapAttrsToList (
      name: value: let
        file = "${config.programs.kodi.datadir}/${name}";
      in "${ensureDir file} && ${xml} edit ${
        lib.strings.concatStringsSep " " (lib.mapAttrsToList (n: v: "--update ${n} --value ${toString v}") value)
      } ${file} > ${file}.tmp && mv ${file}.tmp ${file}"
    )
    data;
in
  pkgs.writeShellScriptBin "create_kodi_settings" ''
    # Let kodi create it's settings if they don't exist

    # kodi_dir="${config.programs.kodi.datadir}"
    # [ ! -d "$kodi_dir" ] && {
    #   ${config.programs.kodi.package}/bin/kodi &
    #   PID=$!
    #   while [ ! -d "$kodi_dir" ]; do :; done
    #   kill $PID
    # }

    ${lib.strings.concatStringsSep "\n" commandList}
  ''
