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
      inherit (lib.lists) flatten singleton;
      inherit (builtins) attrNames;
      # buildAllHosts = map (n: {
      #   name = "Build ${n}";
      #   run = "nix build --accept-flake-config .#nixosConfigurations.${n}.config.system.build.toplevel";
      # }) (attrNames config.flake.nixosConfigurations);

      buildAllHosts = map (n: {
        name = "Build ${n}";
        run = "nix build --accept-flake-config .#nixosConfigurations.${n}.config.system.build.toplevel";
      }) ["server2" "vps" "framework"];

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

        vpsDeploy = {
          name = "Deploy update to vps";

          # other half of the setup in modules/server/deploy.nix
          run = ''
            echo "''${{ secrets.VPS_DEPLOY_SSH_KEY }}" > ~/deploykey
            chmod 600 ~/deploykey

            ssh -t -o BatchMode=yes -o StrictHostKeyChecking=accept-new -i ~/deploykey deploy@kotiboksi.xyz -p 69
          '';
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

      ".github/workflows/build-package.yaml" = {
        name = "Update and build packages";

        on.workflow_dispatch.inputs = {
          package = {
            description = "Package(s) to build";
            required = true;
          };

          flake-input = {
            description = "Flake input(s) to update";
            required = true;
          };

          vps-deploy = {
            description = "Should redeploy vps";
            required = false;
            default = "false";
          };
        };

        jobs.build =
          {
            permissions.contents = "write";
          }
          // mkBasicNix [
            {
              name = "Update \${{ github.event.inputs.flake-input }}";
              run = "nix flake update --accept-flake-config \${{ github.event.inputs.flake-input }}";
            }
            {
              name = "Build \${{ github.event.inputs.package }}";
              run = ''
                for i in ''${{ github.event.inputs.package }}; do
                  nix build .#$i --print-build-logs --accept-flake-config
                done
              '';
            }
            {
              name = "Commit";
              uses = "stefanzweifel/git-auto-commit-action@v5";
              "with" = {
                commit_message = "chore(deps): update \${{ github.event.inputs.flake-input }}";
                commit_user_name = "Flake Bot Update";
                commit_author = "Flake Bot Update <actions@github.com>";
                branch = "main";
                file_pattern = "flake.lock";
                skip_dirty_check = false;
                skip_fetch = true;
              };
            }
          ];

        jobs.update-vps.needs = ["build"];
        jobs.update-vps.steps = singleton (blocks.vpsDeploy
          // {
            "if" = "\${{ inputs.vps-deploy == 'true' }}";
          });
      };

      ".github/workflows/deploy-vps.yaml" = {
        on.workflow_dispatch = {};
        jobs.update-vps.steps = [blocks.vpsDeploy];
      };

      ".github/workflows/mirror.yaml" = {
        name = "Mirror repository for other forges";

        on.push = {};
        jobs.push.steps = [
          blocks.checkout
          {
            name = "Push";
            run = ''
              echo "''${{ secrets.VPS_DEPLOY_SSH_KEY }}" > ~/deploykey
              chmod 600 ~/deploykey

              GIT_SSH_COMMAND="ssh -i ~/deploykey -o StrictHostKeyChecking=no" git push git@tangled.org:thundertheidiot.bsky.social/nixdots main
            '';
          }
        ];
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
              run = ''
                nix-prefetch-url --name CiscoPacketTracer822_amd64_signed.deb https://www.netacad.com/authoring-resources/courses/ff9e491c-49be-4734-803e-a79e6e83dab1/c3636211-1ce6-4f92-8a22-ccddf902dd72/en-US/assets/PacketTracer822_amd64_signed_en-US_35234a27-3127-49bc-91ce-2926af76f07a.deb
                nix-prefetch-url --name displaylink-620.zip https://www.synaptics.com/sites/default/files/exe_files/2025-09/DisplayLink%20USB%20Graphics%20Software%20for%20Ubuntu6.2-EXE.zip
              '';
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
