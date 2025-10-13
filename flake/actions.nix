{
  lib,
  config,
  ...
}: {
  flake.actions-nix = {
    pre-commit.enable = true;

    defaultValues = {
      jobs = {
        runs-on = "ubuntu-latest";
      };
    };

    workflows = let
      inherit (lib.lists) flatten;
      inherit (builtins) attrNames;
      # buildAllHosts = map (n: {
      #   name = "Build ${n}";
      #   run = "nix build --accept-flake-config .#nixosConfigurations.${n}.config.system.build.toplevel";
      # }) (attrNames config.flake.nixosConfigurations);

      buildAllHosts = map (n: {
        name = "Build ${n}";
        run = "nix build --accept-flake-config .#nixosConfigurations.${n}.config.system.build.toplevel";
      }) ["uwu" "vps" "framework"];

      mkBasicNix = list: {
        steps =
          [
            blocks.checkout
            blocks.cleanup
            blocks.nixInstaller
            blocks.cachix
          ]
          ++ (flatten list);
      };

      blocks = {
        checkout = {
          uses = "actions/checkout@v5";
        };

        cleanup = {
          uses = "wimpysworld/nothing-but-nix@v6";
          "with" = {
            hatchet-protocol = "rampage";
            witness-carnage = true;
            nix-permission-edict = true;
          };
        };

        nixInstaller = {
          name = "Nix Installer";
          uses = "cachix/install-nix-action@v31";
          "with" = {
            github_access_token = "\${{ secrets.GITHUB_TOKEN }}";
            install_options = "--no-daemon";
          };
        };

        cachix = {
          name = "Cachix";
          uses = "cachix/cachix-action@v16";
          "with" = {
            name = "meowos";
            authToken = "\${{ secrets.CACHIX_AUTH_TOKEN }}";
          };
        };
      };
    in {
      ".github/workflows/build-hosts.yaml" = {
        on.workflow_dispatch = {};

        jobs.build = mkBasicNix (map (n: {
          name = "Build ${n}";
          run = "nix build --accept-flake-config .#nixosConfigurations.${n}.config.system.build.toplevel";
        }) (attrNames config.flake.nixosConfigurations));
      };

      ".github/workflows/update-flake.yaml" = {
        name = "Update flake.lock";

        on = {
          schedule = [
            {
              cron = "0 03 */4 * *";
            }
          ];
          workflow_dispatch = {};
        };

        jobs.update-lockfile.steps = [
          blocks.checkout
          blocks.nixInstaller
          {
            name = "Update flake.lock";
            run = "nix flake update --accept-flake-config";
          }
          {
            name = "Upload flake.lock";
            uses = "actions/upload-artifact@v4";
            "with" = {
              name = "flake-lock";
              path = "flake.lock";
              retention-days = 1;
            };
          }
        ];

        jobs.build-matrix =
          {
            needs = ["update-lockfile"];
            strategy.matrix.target = let
              inherit (lib) concatStringsSep;
              hostPackage = h: p: "nixosConfigurations.${h}.pkgs.${p}";
              join = concatStringsSep " ";

              vpsPackage = hostPackage "vps";
              vpsPkgs = map vpsPackage;

              frameworkPackage = hostPackage "framework";
              fwPkgs = map frameworkPackage;
            in [
              (join (vpsPkgs ["meowdzbot" "sodexobot"]))
              (vpsPackage "leptos-kotiboksi")
              (join (fwPkgs ["krita" "blender"]))
            ];
          }
          // mkBasicNix [
            {
              name = "Download flake.lock";
              uses = "actions/download-artifact@v4";
              "with".name = "flake-lock";
            }
            {
              name = "Build \${{ matrix.target }}";
              run = ''
                for i in ''${{ matrix.target }}; do
                  nix build .#$i --print-build-logs --accept-flake-config
                done
              '';
            }
          ];

        jobs.update =
          {
            needs = ["build-matrix"];
            permissions.contents = "write";
          }
          // mkBasicNix [
            {
              name = "Download flake.lock";
              uses = "actions/download-artifact@v4";
              "with".name = "flake-lock";
            }
            {
              name = "Prefetch displaylink";
              # TODO update with displaylink
              run = "nix-prefetch-url --name displaylink-620.zip https://www.synaptics.com/sites/default/files/exe_files/2025-09/DisplayLink%20USB%20Graphics%20Software%20for%20Ubuntu6.2-EXE.zip";
            }
            buildAllHosts
            {
              name = "Commit";
              uses = "stefanzweifel/git-auto-commit-action@v5";
              "with" = {
                commit_message = "chore(deps): bump flake.lock";
                commit_user_name = "Flake Bot Update";
                commit_author = "Flake Bot Update <actions@github.com>";
                branch = "main";
                file_pattern = "flake.lock";
                skip_dirty_check = false;
                skip_fetch = true;
              };
            }
          ];
      };
    };
  };
}
