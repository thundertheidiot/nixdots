{
  lib,
  stdenv,
  xml2,
  makeWrapper,
  callPackage,
  kodi-wayland,
  python312Packages,
  glib,
  nspr,
  nss,
}: let
  pythonPath = with python312Packages; makePythonPath [pillow pycryptodome urllib3 certifi six webencodings chardet charset-normalizer idna six dateutil];
in
  stdenv.mkDerivation rec {
    name = "kodi_with_addons";

    srcs = callPackage ./plugins.nix {};

    # this needs to happen for some reason
    unpackPhase = "true";

    nativeBuildInputs = [xml2 makeWrapper];

    installPhase = ''
      dir="$out/addons"
      mkdir -p "$dir"
      addonPythonPath=""
      for srcFile in $srcs; do
        name=$("${xml2}/bin/xml2" < "$srcFile"/addon.xml | grep '/addon/@id=' | sed 's/\/addon\/@id=//g')
        mkdir -p "$dir/$name"
        cp --dereference --recursive "$srcFile"/* "$dir/$name/"
        [ -d "$dir/$name/lib" ] && addonPythonPath="$addonPythonPath:$dir/$name/lib"
        [ -d "$dir/$name/libs" ] && addonPythonPath="$addonPythonPath:$dir/$name/libs"
        [ -d "$dir/$name/resources/lib" ] && addonPythonPath="$addonPythonPath:$dir/$name/resources/lib"
      done

      mkdir -p "$out/share/kodi/userdata/addon_data"

      mkdir -p "$out/bin"
      makeWrapper "${kodi-wayland}/bin/kodi" "$out/bin/kodi_with_addons" \
       --prefix PYTHONPATH : ${pythonPath}:$addonPythonPath \
       --prefix LD_LIBRARY_PATH ":" "${lib.makeLibraryPath [glib nspr nss stdenv.cc.cc.lib]}:$dir/inputstream.adaptive"
    '';
  }
