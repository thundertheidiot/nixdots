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
        XR_RUNTIME_JSON="$(realpath "$XDG_CONFIG_HOME/openxr/1/active_runtime.json")"
        export XR_RUNTIME_JSON
        export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [ pkgs.openxr-loader ]}:$LD_LIBRARY_PATH"
        export PRESSURE_VESSEL_IMPORT_OPENXR_1_RUNTIMES=1
        export PRESSURE_VESSEL_FILESYSTEMS_RW="/run/user/1000/monado_comp_ipc"
        export PRESSURE_VESSEL_FILESYSTEMS="/nix/store"
      '';
      wrapper = "env PROTON_LOG=1";
      command = ''"$HOME/BSManager/BSInstances/Beat Saber/Beat Saber.exe"'';
    })
  ];
}
