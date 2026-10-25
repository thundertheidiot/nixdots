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
        inherit (lib.lists) flatten;
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
            uses = "actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1"; # v7
            "with".persist-credentials = false;
          };

          cleanup = {
            uses = "wimpysworld/nothing-but-nix@687c797a730352432950c707ab493fcc951818d7"; # v10
            "with" = {
              hatchet-protocol = "rampage";
              nix-permission-edict = true;
            };
          };

          nixInstaller = {
            name = "Nix Installer";
            uses = "cachix/install-nix-action@75c75c31ebece974e48ebc36b4ad125bd97bd97b"; # v31
            "with" = {
              github_access_token = "\${{ secrets.GITHUB_TOKEN }}";
              install_options = "--no-daemon";
            };
          };

          cachix = {
            name = "Cachix";
            uses = "cachix/cachix-action@38b082610b782e7e93e209c35fd730d399dee866"; # v17
            "with" = {
              name = "meowos";
              authToken = "\${{ secrets.CACHIX_AUTH_TOKEN }}";
            };
          };

          pr = args: {
            name = args.name;
            uses = "peter-evans/create-pull-request@22a9089034f40e5a961c8808d113e2c98fb63676"; # v7
            "with" = builtins.removeAttrs args [ "name" ];
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
          permissions.contents = "read";

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
              contents = "read";
            };
            steps = [
              blocks.checkout
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
                    for attempt in 1 2 3; do
                      if nix-update --flake --use-github-releases "$package" "''${args[@]}"; then
                        break
                      fi
                      if [[ "$attempt" == 3 ]]; then
                        printf '::error::Failed to update %s after three attempts\n' "$package"
                        exit 1
                      fi
                      printf '::warning::Update for %s failed; retrying in 10 seconds\n' "$package"
                      sleep 10
                    done

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
                name = "Stage package updates";
                run = ''
                  mkdir -p "$RUNNER_TEMP/package-updates"
                  cp pkgs/{helium,glide,photocraft,sable-desktop,dgr}.nix "$RUNNER_TEMP/package-updates/"
                  cp "$PR_BODY" "$RUNNER_TEMP/package-updates/package-updates.md"
                '';
              }
              {
                name = "Upload package updates";
                uses = "actions/upload-artifact@ea165f8d65b6e75b540449e92b4886f43607fa02"; # v4
                "with" = {
                  name = "package-updates";
                  path = "\${{ runner.temp }}/package-updates";
                  retention-days = 1;
                };
              }
            ];
          };

          jobs.create-pr = {
            needs = [ "update" ];
            permissions = {
              contents = "write";
              pull-requests = "write";
            };
            steps = [
              blocks.checkout
              {
                name = "Download package updates";
                uses = "actions/download-artifact@d3f86a106a0bac45b974a628896c90dbdf5c8093"; # v4
                "with" = {
                  name = "package-updates";
                  path = "\${{ runner.temp }}/package-updates";
                };
              }
              {
                name = "Apply package updates";
                run = "cp \"$RUNNER_TEMP\"/package-updates/*.nix pkgs/";
              }
              (blocks.pr {
                name = "Open combined update PR";
                branch = "updates/custom-packages";
                delete-branch = true;
                commit-message = "chore(deps): update custom packages";
                title = "chore(deps): update custom packages";
                body-path = "\${{ runner.temp }}/package-updates/package-updates.md";
                add-paths = ''
                  pkgs/helium.nix
                  pkgs/glide.nix
                  pkgs/photocraft.nix
                  pkgs/sable-desktop.nix
                  pkgs/dgr.nix
                '';
              })
            ];
          };
        };

        ".github/workflows/build-hosts.yaml" = {
          on.workflow_dispatch = { };
          permissions.contents = "read";

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

          };

          jobs.build = {
            permissions.contents = "read";
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
              name = "Upload updated flake.lock";
              uses = "actions/upload-artifact@ea165f8d65b6e75b540449e92b4886f43607fa02"; # v4
              "with" = {
                name = "manual-flake-lock";
                path = "flake.lock";
                retention-days = 1;
              };
            }
          ];

          jobs.create-pr = {
            needs = [ "build" ];
            permissions = {
              contents = "write";
              pull-requests = "write";
            };
            steps = [
              blocks.checkout
              {
                name = "Download updated flake.lock";
                uses = "actions/download-artifact@d3f86a106a0bac45b974a628896c90dbdf5c8093"; # v4
                "with".name = "manual-flake-lock";
              }
              (blocks.pr {
                name = "Open flake update PR";
                add-paths = "flake.lock";
                branch = "updates/manual-flake-inputs";
                commit-message = "chore(deps): update flake inputs";
                delete-branch = true;
                title = "chore(deps): update flake inputs";
                body = "Flake inputs and the selected package builds were updated by the manual workflow.";
              })
            ];
          };
        };

        ".github/workflows/deploy-vps.yaml" = {
          on.workflow_dispatch = { };
          permissions = { };
          jobs.update-vps.steps = [ (blocks.vpsDeploy "\${{ github.sha }}") ];
        };

        ".github/workflows/update-flake.yaml" = {
          name = "Update flake.lock";
          concurrency = lockUpdateConcurrency;
          permissions.contents = "read";

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
              uses = "actions/upload-artifact@ea165f8d65b6e75b540449e92b4886f43607fa02"; # v4
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
              uses = "actions/download-artifact@d3f86a106a0bac45b974a628896c90dbdf5c8093"; # v4
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
            permissions.contents = "read";
          }
          // mkBasicNix [
            {
              name = "Download flake.lock";
              uses = "actions/download-artifact@d3f86a106a0bac45b974a628896c90dbdf5c8093"; # v4
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
              name = "Upload verified flake.lock";
              uses = "actions/upload-artifact@ea165f8d65b6e75b540449e92b4886f43607fa02"; # v4
              "with" = {
                name = "verified-flake-lock";
                path = "flake.lock";
                retention-days = 1;
              };
            }
          ];

          jobs.create-pr = {
            needs = [ "update" ];
            permissions = {
              contents = "write";
              pull-requests = "write";
            };
            steps = [
              (blocks.checkout // { "with".persist-credentials = false; })
              {
                name = "Download verified flake.lock";
                uses = "actions/download-artifact@d3f86a106a0bac45b974a628896c90dbdf5c8093"; # v4
                "with".name = "verified-flake-lock";
              }
              (blocks.pr {
                name = "Open flake update PR";
                add-paths = "flake.lock";
                branch = "updates/flake-lock";
                commit-message = "chore(deps): bump flake.lock";
                delete-branch = true;
                title = "chore(deps): bump flake.lock";
                body = "Automated flake input update. The configured package and host builds passed.";
              })
            ];
          };
        };
      };
  };
}
