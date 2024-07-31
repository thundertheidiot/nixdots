{
  config,
  lib,
  pkgs,
  mpkgs,
  ...
}:
lib.mkIf (config.setup.gaming.enable) (with config; {
  home.packages = with pkgs;
    [
      vesktop
      webcord # fallback
    ];

  home.sessionVariables.MANGOHUD_CONFIGFILE = "${xdg.configHome}/mangohud.conf";
  systemd.user.sessionVariables.MANGOHUD_CONFIGFILE = "${xdg.configHome}/mangohud.conf";
  home.sessionVariables.MANGOHUD_PRESETSFILE = "${xdg.configHome}/mangohud_presets.conf";
  systemd.user.sessionVariables.MANGOHUD_PRESETSFILE = "${xdg.configHome}/mangohud_presets.conf";

  xdg.configFile."mangohud_presets.conf" = {
    enable = true;
    text = ''
      [preset 1]
      no_display

      [preset 2]
      cpu_stats=0
      gpu_stats=0
      fps
      fps_only=1
      frametime=0

      [preset 3]
      cpu_stats=0
      gpu_stats=0
      fps
      fps_only=0
      frametime=1

      [preset 4]
      cpu_stats=1
      cpu_temp
      gpu_stats=1
      gpu_temp
      vulkan_driver
      ram
      vram
      fps
      frametime=1

      [preset 5]
      cpu_stats=1
      cpu_temp
      cpu_power
      gpu_stats=1
      gpu_temp
      gpu_power
      vulkan_driver
      ram
      vram
      core_load
      fps
      frametime=1
    '';
  };

  xdg.configFile."mangohud.conf" = {
    enable = true;
    text = ''
      toggle_hud=Shift_L+F1
      toggle_hud_position=Shift_L+F2
      reload_cfg=Shift_L+F4
      toggle_preset=Shift_L+F3
      position=middle-left
      background_alpha=0.2
      preset=4
    '';
  };
})
