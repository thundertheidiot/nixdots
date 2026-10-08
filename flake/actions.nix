{
  lib,
  config,
  ...
}:
{
  flake.actions-nix = {
    pre-commit.enable = true;

    defaultValues = {
      jobs = {
        runs-on = "ubuntu-latest";
      };
    };

    workflows =
      let
        inherit (lib.lists) flatten singleton;
        inherit (builtins) attrNames;
        lockUpdateConcurrency = {
          group = "update-flake-lock";
          cancel-in-progress = false;
        };
        # buildAllHosts = map (n: {
        #   name = "Build ${n}";
        #   run = "nix build --accept-flake-config .#nixosConfigurations.${n}.config.system.build.toplevel";
        # }) (attrNames config.flake.nixosConfigurations);

        buildAllHosts =
          map
            (n: {
              name = "Build ${n}";
              run = "nix build --accept-flake-config .#nixosConfigurations.${n}.config.system.build.toplevel";
            })
            [
              "server2"
              "vps"
              "framework"
            ];

        mkBasicNix = list: {
          steps = [
            blocks.checkout
            blocks.cleanup
            blocks.nixInstaller
            blocks.cachix
          ]
          ++ (flatten list);
        };

        blocks = {
          checkout = {
            uses = "actions/checkout@v7";
          };

          cleanup = {
            uses = "wimpysworld/nothing-but-nix@v10";
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
            uses = "cachix/cachix-action@v17";
            "with" = {
              name = "meowos";
              authToken = "\${{ secrets.CACHIX_AUTH_TOKEN }}";
            };
          };

          vpsDeploy = revision: {
            name = "Deploy update to vps";
            env = {
              DEPLOY_REVISION = revision;
              DEPLOY_SSH_KEY = "\${{ secrets.VPS_DEPLOY_SSH_KEY }}";
            };

            # other half of the setup in modules/server/deploy.nix
            run = ''
              [[ "$DEPLOY_REVISION" =~ ^[0-9a-f]{40}$ ]]
              umask 077
              printf '%s\n' "$DEPLOY_SSH_KEY" > ~/deploykey
              chmod 600 ~/deploykey

              ssh -T -o BatchMode=yes -o StrictHostKeyChecking=accept-new -i ~/deploykey -p 69 deploy@kotiboksi.xyz "$DEPLOY_REVISION"
            '';
          };
        };
      in
      {
        ".github/workflows/update-packages.yaml" = {
          name = "Update custom packages";

          on = {
            schedule = [ { cron = "23 4 * * *"; } ];
            workflow_dispatch = { };
          };

          concurrency = {
            group = "update-custom-packages";
            cancel-in-progress = false;
          };

          jobs.update = {
            permissions = {
              contents = "write";
              pull-requests = "write";
            };
            steps = [
              (blocks.checkout // { "with".persist-credentials = false; })
              blocks.cleanup
              blocks.nixInstaller
              {
                name = "Update and build packages";
                env = {
                  GITHUB_TOKEN = "\${{ secrets.GITHUB_TOKEN }}";
                  PR_BODY = "\${{ runner.temp }}/package-updates.md";
                };
                run = ''
                  nix develop --accept-flake-config --command bash -euo pipefail <<'SCRIPT'
                  printf "Automated custom package updates. Each changed package was built successfully.\n\n" > "$PR_BODY"

                  for package in helium glide photocraft sable-desktop dgr; do
                    old_version=$(nix eval --raw ".#packages.x86_64-linux.$package.version")
                    args=()
                    # Glide uses prerelease-style tags for its regular releases.
                    if [[ "$package" == glide ]]; then
                      args+=(--version=unstable)
                    fi
                    nix-update --flake "$package" "''${args[@]}"

                    if ! git diff --quiet -- "pkgs/$package.nix"; then
                      nix build --accept-flake-config --no-link --print-build-logs ".#$package"
                      new_version=$(nix eval --raw ".#packages.x86_64-linux.$package.version")
                      printf -- "- %s: %s -> %s\n" "$package" "$old_version" "$new_version" >> "$PR_BODY"
                    fi
                  done
                  SCRIPT
                '';
              }
              {
                name = "Open combined update PR";
                uses = "peter-evans/create-pull-request@v7";
                "with" = {
                  branch = "updates/custom-packages";
                  delete-branch = true;
                  commit-message = "chore(deps): update custom packages";
                  title = "chore(deps): update custom packages";
                  body-path = "\${{ runner.temp }}/package-updates.md";
                  add-paths = ''
                    pkgs/helium.nix
                    pkgs/glide.nix
                    pkgs/photocraft.nix
                    pkgs/sable-desktop.nix
                    pkgs/dgr.nix
                  '';
                };
              }
            ];
          };
        };

        ".github/workflows/build-hosts.yaml" = {
          on.workflow_dispatch = { };

          jobs.build = mkBasicNix buildAllHosts;
        };

        ".github/workflows/build-package.yaml" = {
          name = "Update and build packages";
          concurrency = lockUpdateConcurrency;

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

          jobs.build = {
            permissions.contents = "write";
            outputs.revision = "\${{ steps.revision.outputs.revision }}";
          }
          // mkBasicNix [
            {
              name = "Update \${{ github.event.inputs.flake-input }}";
              env.FLAKE_INPUTS = "\${{ inputs.flake-input }}";
              run = ''
                read -r -a inputs <<< "$FLAKE_INPUTS"
                (( ''${#inputs[@]} > 0 ))
                nix flake update --accept-flake-config -- "''${inputs[@]}"
              '';
            }
            {
              name = "Build \${{ github.event.inputs.package }}";
              env.PACKAGES = "\${{ inputs.package }}";
              run = ''
                read -r -a packages <<< "$PACKAGES"
                (( ''${#packages[@]} > 0 ))
                for package in "''${packages[@]}"; do
                  nix build ".#$package" --print-build-logs --accept-flake-config
                done
              '';
            }
            {
              name = "Commit";
              uses = "stefanzweifel/git-auto-commit-action@v7";
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
            {
              name = "Record committed revision";
              id = "revision";
              run = ''
                printf 'revision=%s\n' "$(git rev-parse HEAD)" >> "$GITHUB_OUTPUT"
              '';
            }
          ];

          jobs.update-vps.needs = [ "build" ];
          jobs.update-vps.steps = singleton (
            (blocks.vpsDeploy "\${{ needs.build.outputs.revision }}")
            // {
              "if" = "\${{ inputs.vps-deploy == 'true' }}";
            }
          );
        };

        ".github/workflows/deploy-vps.yaml" = {
          on.workflow_dispatch = { };
          jobs.update-vps.steps = [ (blocks.vpsDeploy "\${{ github.sha }}") ];
        };

        ".github/workflows/update-flake.yaml" = {
          name = "Update flake.lock";
          concurrency = lockUpdateConcurrency;

          on = {
            schedule = [
              {
                cron = "0 03 */4 * *";
              }
            ];
            workflow_dispatch = { };
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

          jobs.build-matrix = {
            needs = [ "update-lockfile" ];
            strategy.matrix.target =
              let
                inherit (lib) concatStringsSep;
                hostPackage = h: p: "nixosConfigurations.${h}.pkgs.${p}";
                join = concatStringsSep " ";

                vpsPackage = hostPackage "vps";
                vpsPkgs = map vpsPackage;
                # frameworkPackage = hostPackage "framework";
                # fwPkgs = map frameworkPackage;
              in
              [
                (join (vpsPkgs [
                  "meowdzbot"
                  "sodexobot"
                ]))
                (vpsPackage "leptos-kotiboksi")
                # (join (fwPkgs ["krita" "blender"]))
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

          jobs.update = {
            needs = [ "build-matrix" ];
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
              run = ''
                nix develop --accept-flake-config -c just prefetch
              '';
            }
            buildAllHosts
            {
              name = "Commit";
              uses = "stefanzweifel/git-auto-commit-action@v7";
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
