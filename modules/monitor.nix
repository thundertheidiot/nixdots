{
  system = {
    config,
    lib,
    pkgs,
    ...
  }: let
    inherit (builtins) filter isPath length;

    edids = filter (s: s != {}) (map (
        s:
          with s;
            if isPath edid
            then {inherit name edid;}
            else {}
      )
      config.monitors);
  in
    lib.mkMerge [
      (lib.mkIf ((length edids) != 0) {
        hardware.firmware = map (s: with s; (pkgs.runCommandNoCC "${name}-edid-override" {compressFirmware = false;} ''
          mkdir -p "$out/lib/firmware/edid"
          cp "${edid}" "$out/lib/firmware/edid/${name}.bin"
        '')) edids;

        boot.kernelParams =
          map (
            s: with s; "drm.edid_firmware=${name}:edid/${name}.bin"
          )
          edids;

        environment.systemPackages = [
          (pkgs.callPackage ../pkgs/cru.nix {})
        ];
      })
    ];

  home = {
    config,
    lib,
    pkgs,
    inputs,
    mlib,
    ...
  }: let
    inherit (builtins) elem;
  in
    lib.mkMerge [
      (lib.mkIf (elem "hyprland" config.workstation.environment) (let
        splitMonitorWorkspaces = config.setup.hyprland.forceMultiMonitor || (builtins.length config.monitors) > 1;
        hyprWorkspace =
          if splitMonitorWorkspaces
          then "split-workspace"
          else "workspace";
        hyprMoveToWorkspaceSilent =
          if splitMonitorWorkspaces
          then "split-movetoworkspacesilent"
          else "movetoworkspacesilent";
      in {
        wayland.windowManager.hyprland = {
          plugins = lib.mkIf splitMonitorWorkspaces [pkgs.hyprland-split-monitor-workspaces];

          settings = {
            plugin.split-monitor-workspaces = {
              count = 10;
              keep_focused = 1;
            };

            monitor = lib.mkIf (config.monitors != []) (builtins.map (m:
              with m;
                lib.mkIf (!hyprlandExclude)
                "${name}, ${width}x${height}@${refresh}, ${x}x${y}, 1${
                  if (hyprlandExtra != "")
                  then ", ${hyprlandExtra}"
                  else ""
                }")
            config.monitors);

            workspace = lib.mkIf (!splitMonitorWorkspaces && config.monitors != []) (
              builtins.map (n: let
                mon = (builtins.head config.monitors).name;
              in "${builtins.toString n}, monitor:${mon}")
              [1 2 3 4 5 6 7 8 9]
            );

            bind = [
              "$mod, 1, ${hyprWorkspace}, 1"
              "$mod, 2, ${hyprWorkspace}, 2"
              "$mod, 3, ${hyprWorkspace}, 3"
              "$mod, 4, ${hyprWorkspace}, 4"
              "$mod, 5, ${hyprWorkspace}, 5"
              "$mod, 6, ${hyprWorkspace}, 6"
              "$mod, 7, ${hyprWorkspace}, 7"
              "$mod, 8, ${hyprWorkspace}, 8"
              "$mod, 9, ${hyprWorkspace}, 9"
              "$mod, 0, ${hyprWorkspace}, 0"

              "$shiftmod, 1, ${hyprMoveToWorkspaceSilent}, 1"
              "$shiftmod, 2, ${hyprMoveToWorkspaceSilent}, 2"
              "$shiftmod, 3, ${hyprMoveToWorkspaceSilent}, 3"
              "$shiftmod, 4, ${hyprMoveToWorkspaceSilent}, 4"
              "$shiftmod, 5, ${hyprMoveToWorkspaceSilent}, 5"
              "$shiftmod, 6, ${hyprMoveToWorkspaceSilent}, 6"
              "$shiftmod, 7, ${hyprMoveToWorkspaceSilent}, 7"
              "$shiftmod, 8, ${hyprMoveToWorkspaceSilent}, 8"
              "$shiftmod, 9, ${hyprMoveToWorkspaceSilent}, 9"
              "$shiftmod, 0, ${hyprMoveToWorkspaceSilent}, 0"
            ];
          };
        };
      }))
    ];
}
