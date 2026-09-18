{ pkgs, ... }:
let
  winepkgs = pkgs.winepkgs;
  winelib = winepkgs.lib;
in
{
  environment.systemPackages = [
    (winelib.protonApp {
      name = "beatsaber";
      proton = winepkgs.protonGE."10-34";
      extraSteps = [
        ''
          wine reg add "HKLM\\SOFTWARE\\Khronos\\OpenXR\\1" /v ActiveRuntime /t REG_SZ /d "C:\\openxr\\wineopenxr64.json" /f
        ''
      ];
      extraSetup = ''
        export OXR_NO_TEXTURE_SOURCE_ALPHA=1
      '';
      wrapper = "vrhelper game";
      command = ''"$HOME/BSManager/BSInstances/Beat Saber/Beat Saber.exe"'';
    })
    (winelib.protonApp {
      name = "fnafhw";
      extraSetup = ''
        cd "$HOME/Games/fnafhw/FNAFVRHelpWanted"
      '';
      proton = winepkgs.protonGE."10-34";
      wrapper = "vrhelper game";
      command = ''"$HOME/Games/fnafhw/FNAFVRHelpWanted/freddys.exe"'';
    })
    (winelib.protonApp rec {
      name = "sh2vr";

      proton = winepkgs.protonGE."10-34";

      extraSteps =
        let
          sh2uevr = pkgs.fetchzip {
            url = "https://github.com/jbusfield/SH2_UEVR/releases/download/v1.0.2/SHProto-Win64-Shipping.zip";
            stripRoot = false;
            hash = "sha256-C6Vo0+ZR9LIKTZKp9MxhHj/nbAiJBQO6T7hOS8liMgo=";
          };
        in
        [
          (winelib.winetricks "dotnetdesktop6")
          (pkgs.writeShellScript "installuevrprofile" ''
            mkdir -p "$WINEPREFIX/drive_c/users/steamuser/AppData/Roaming/UnrealVRMod/SHProto-Win64-Shipping"
            cp -r ${sh2uevr}/* "$WINEPREFIX/drive_c/users/steamuser/AppData/Roaming/UnrealVRMod/SHProto-Win64-Shipping/"
          '')
        ];

      filespec = ''
        drive_c/users/steamuser/AppData/Roaming/UnrealVRMod/SHProto-Win64-Shipping/config.txt
      '';

      extraSetup = ''
        export UMU_NO_PROTON=1
      '';

      wrapper = "vrhelper game";

      command =
        let
          uevr_afw = pkgs.fetchzip {
            url = "https://github.com/PureDark/UEVR/releases/download/UEVR_AFW_v1.0-beta.3/UEVR-nightly_AFW_v1.0-beta.3.1.zip";
            hash = "sha256-wzail7jGZZqlBm0vXwqUullTlgb/NCR53Yu3LpBbd4c=";
            stripRoot = false;
          };
        in
        pkgs.writeShellScript "run" ''
          "${proton}/proton" runinprefix "/mnt/1tb_nvme/games/Silent Hill 2/SHProto.exe" &
          sleep 5
          "${proton}/proton" runinprefix "${uevr_afw}/UEVRInjector.exe"
        '';
    })
  ];
}
