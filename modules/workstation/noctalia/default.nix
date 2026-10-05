{
  config,
  lib,
  mlib,
  pkgs,
  ...
}:
let
  inherit (lib)
    flatten
    mkIf
    mkMerge
    mkOption
    mapAttrs
    mapAttrsToList
    removeSuffix
    listToAttrs
    nameValuePair
    ;
  inherit (mlib) mkEnOpt mkOpt homeModule;
  inherit (lib.types)
    listOf
    attrsOf
    submodule
    path
    str
    int
    ;

  work = config.meow.workstation.enable;
  cfg = config.meow.noctalia.enable;

  widgetFiles =
    widget:
    map (file: {
      inherit file;
      path = baseNameOf (toString file);
      id = removeSuffix ".luau" (baseNameOf (toString file));
    }) widget.widget;

  widgetPlugin =
    widget:
    let
      files = widgetFiles widget;
      manifest =
        (removeAttrs widget [
          "postRun"
          "widget"
        ])
        // {
          widget = map (file: {
            inherit (file) id;
            entry = file.path;
          }) files;
        };
    in
    pkgs.runCommand "noctalia-widget-${widget.name}" { } ''
      mkdir -p "$out"
      ${lib.concatMapStringsSep "\n" (file: ''
        cp -R ${file.file} "$out/${file.path}"
      '') files}
      cp ${pkgs.writers.writeTOML "plugin.toml" manifest} "$out/plugin.toml"
      ${widget.postRun}
    '';

  widgetPlugins = mapAttrs (_: widget: widgetPlugin widget) config.meow.noctalia.widgets;

  widgetInstances = flatten (
    mapAttrsToList (
      name: widget:
      map (file: {
        instance = file.id;
        type = "${widget.id}:${file.id}";
      }) (widgetFiles widget)
    ) config.meow.noctalia.widgets
  );
in
{
  options.meow.noctalia = {
    enable = mkEnOpt "Noctalia shell";
    widgets = mkOption {
      description = "Additional widgets";
      default = { };
      type = attrsOf (
        submodule (
          { name, ... }: {
            options = {
              name = mkOpt str name { };
              id = mkOpt str "local/${name}" { };
              version = mkOpt str "1.0.0" { };
              plugin_api = mkOpt int 3 { };
              description = mkOpt str "description" { };

              postRun = mkOpt str "" { };

              widget = mkOpt (listOf path) [ ] { };
            };
          }
        )
      );

    };
  };

  config = mkIf (work && cfg) (mkMerge [
    {
      programs.noctalia = {
        enable = true;
        recommendedServices.enable = true;
      };
    }
    (homeModule {
      programs.noctalia = {
        enable = true;
        settings = {
          theme = {
            source = "builtin";
            builtin = "Catppuccin";
          };

          plugins.enabled = mapAttrsToList (_: widget: widget.id) config.meow.noctalia.widgets;

          bar.default = {
            start = [ "workspaces" ];
            center = [ "clock" ];
            end = map (widget: widget.instance) widgetInstances ++ [
              "network"
              "notifications"
              "clipboard"
              "bluetooth"
              "volume"
              "brightness"
              "battery"
              "tray"
              "control-center"
              "session"
            ];
          };

          widget = listToAttrs (
            map (widget: nameValuePair widget.instance { type = widget.type; }) widgetInstances
          );
        };
      };

      xdg.dataFile = lib.mapAttrs' (name: source: {
        name = "noctalia/plugins/${name}";
        value.source = source;
      }) widgetPlugins;
    })
  ]);
}
