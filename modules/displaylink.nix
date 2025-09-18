{
  mlib,
  lib,
  config,
  pkgs,
  ...
}: let
  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf;

  cfg = config.meow.displaylink;
in {
  options.meow.displaylink = mkEnOpt "DisplayLink support";

  config = mkIf cfg {
    environment.systemPackages = [
      pkgs.displaylink
    ];

    services.xserver.videoDrivers = ["displaylink" "modesetting"];

    boot = {
      extraModulePackages = [
        config.boot.kernelPackages.evdi
        # (config.boot.kernelPackages.evdi.overrideAttrs (_: {
        #   env.NIX_CFLAGS_COMPILE = toString [
        #     "-Wno-error"
        #     "-Wno-error=sign-compare"
        #   ];

        #   buildInputs = [
        #     config.boot.kernelPackages.kernel
        #     pkgs.libdrm
        #     (
        #       pkgs.python3.withPackages
        #       (ps: [
        #         (ps.pybind11.overrideAttrs
        #           (_: {
        #             doCheck = false;
        #           }))
        #       ])
        #     )
        #   ];
        # }))
      ];

      initrd.kernelModules = ["evdi"];
    };
  };
}
