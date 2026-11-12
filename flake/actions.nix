{
  lib,
  config,
  ...
}:
{
  # markdown summary of flake.lock input changes and optional host closure diffs
  perSystem =
    { pkgs, ... }:
    let
      lockDiff = pkgs.writeText "lock-diff.jq" ''
        def locked($l; $n): $l.nodes[$l.nodes.root.inputs[$n] // ""].locked // null;
        def fmt: if . == null then "-" else "`\(.rev // .narHash | .[0:7])` \(.lastModified | strftime("%Y-%m-%d"))" end;
        def repo: if .type == "github" then "\(.owner)/\(.repo)"
          elif (.url // "" | test("nixos\\.org")) then "NixOS/nixpkgs"
          else null end;

        [ ($old[0].nodes.root.inputs + $new[0].nodes.root.inputs | keys[]) as $n
          | locked($old[0]; $n) as $o
          | locked($new[0]; $n) as $w
          | select($o.narHash != $w.narHash)
          | "| \($n) | \($o | fmt) | \($w | fmt) | \(
              if $o != null and $w != null and ($w | repo) != null and $o.rev != null
              then "[compare](https://github.com/\($w | repo)/compare/\($o.rev)...\($w.rev))"
              else "" end) |"
        ]
        | if length == 0 then "No flake input changes."
          else (["| Input | Old | New | |", "|---|---|---|---|"] + .) | join("\n") end
      '';
    in
    {
      packages.flake-diff = pkgs.writeShellApplication {
        name = "flake-diff";
        runtimeInputs = with pkgs; [
          jq
          nvd
        ];
        # usage: flake-diff OLD_LOCK NEW_LOCK [CLOSURES], closures has old/<name> and new/<name> links
        text = ''
          printf '### Flake inputs\n\n'
          jq -rn --slurpfile old "$1" --slurpfile new "$2" -f ${lockDiff}

          closures=''${3:-}
          [[ -n "$closures" && -d "$closures/new" ]] || exit 0

          printf '\n### Closure changes\n'
          for new in "$closures"/new/*; do
            name=$(basename "$new")
            old="$closures/old/$name"
            [[ -e "$old" ]] || continue

            diff=$(nvd --color never diff "$old" "$new")
            lines=$(wc -l <<< "$diff")
            printf '\n<details><summary><b>%s</b>: %s</summary>\n\n```\n' "$name" "$(tail -n 1 <<< "$diff")"
            head -n 300 <<< "$diff"
            (( lines > 300 )) && printf '... %d more lines\n' "$(( lines - 300 ))"
            printf '```\n\n</details>\n'
          done
        '';
      };
    };

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
        inherit (lib) concatStringsSep optionalAttrs;
        inherit (builtins) attrNames attrValues;

        # input -> packages built and cached after updating it
        frequentInputs = {
          albedo = [
            "nixosConfigurations.framework.config.home-manager.users.thunder.programs.albedo.package"
          ];
        };
        lockUpdateConcurrency = {
          group = "update-flake-lock";
          cancel-in-progress = false;
        };
        # buildAllHosts = map (n: {
        #   name = "Build ${n}";
        #   run = "nix build --accept-flake-config .#nixosConfigurations.${n}.config.system.build.toplevel";
        # }) (attrNames config.flake.nixosConfigurations);

        hosts = [
          "server2"
          "vps"
          "framework"
        ];
        buildHost =
          n: "nix build --accept-flake-config .#nixosConfigurations.${n}.config.system.build.toplevel";

        buildAllHosts = map (n: {
          name = "Build ${n}";
          run = buildHost n;
        }) hosts;

        # links closures for flake-diff
        buildClosures =
          state:
          map (
            n:
            {
              name = "Build ${n} (${state})";
              run = ''${buildHost n} --out-link "$RUNNER_TEMP/closures/${state}/${n}"'';
            }
            # old builds only feed the diff
            // optionalAttrs (state == "old") { continue-on-error = true; }
          ) hosts;

        flakeDiff = ''nix run --accept-flake-config .#flake-diff -- "$RUNNER_TEMP/old.lock" flake.lock'';
        saveOldLock = ''git show HEAD:flake.lock > "$RUNNER_TEMP/old.lock"'';

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

          concurrency = lockUpdateConcurrency;

          jobs.update = {
            permissions = {
              contents = "read";
            };
          }
          // mkBasicNix [
            {
              name = "Update frequent flake inputs";
              env.FLAKE_INPUTS = concatStringsSep " " (attrNames frequentInputs);
              run = ''
                if [[ -n "$FLAKE_INPUTS" ]]; then
                  read -r -a inputs <<< "$FLAKE_INPUTS"
                  nix flake update --accept-flake-config -- "''${inputs[@]}"
                fi
              '';
            }
            {
              name = "Build frequent flake input packages";
              env.PACKAGES = concatStringsSep " " (flatten (attrValues frequentInputs));
              run = ''
                if git diff --quiet -- flake.lock; then
                  exit 0
                fi
                read -r -a packages <<< "$PACKAGES"
                for package in "''${packages[@]}"; do
                  if ! nix build --accept-flake-config --no-link --print-build-logs ".#$package"; then
                    printf '::warning::Failed to build %s, dropping frequent flake input updates\n' "$package"
                    git checkout -- flake.lock
                    exit 0
                  fi
                done
              '';
            }
            {
              name = "Update and build packages";
              env = {
                GITHUB_TOKEN = "\${{ secrets.GITHUB_TOKEN }}";
                PR_BODY = "\${{ runner.temp }}/package-updates.md";
              };
              run = ''
                nix develop --accept-flake-config --command bash -euo pipefail <<'SCRIPT'
                printf "Automated custom package updates. Each changed package was built successfully.\n\n" > "$PR_BODY"
                if ! git diff --quiet -- flake.lock; then
                  printf -- "- Frequent flake inputs updated, their packages were built and cached.\n" >> "$PR_BODY"
                fi

                policies=$(nix eval --json --accept-flake-config .#packageUpdates)
                packages=$(jq -r 'keys[]' <<< "$policies")
                for package in $packages; do
                  policy=$(jq -c --arg package "$package" '.[$package]' <<< "$policies")
                  reason=$(jq -r '.skip // empty' <<< "$policy")
                  if [[ -n "$reason" ]]; then
                    printf 'Skipping %s: %s\n' "$package" "$reason"
                    continue
                  fi
                  old_version=$(nix eval --raw ".#packages.x86_64-linux.$package" --apply 'p: p.version or "local"')
                  mapfile -t args < <(jq -r '.args[]' <<< "$policy")
                  git diff --binary -- pkgs/ > "$RUNNER_TEMP/package-before.patch"
                  update_package() {
                    if [[ $(jq -r '.script' <<< "$policy") == true ]]; then
                      updater=$(nix build --no-link --print-out-paths ".#packages.x86_64-linux.$package.autoUpdate.script") || return
                      UPDATE_NIX_ATTR_PATH="$package" "$updater"/bin/*
                    else
                      nix-update --flake --use-github-releases "$package" "''${args[@]}"
                    fi
                  }
                  for attempt in 1 2 3; do
                    if update_package; then
                      break
                    fi
                    if [[ "$attempt" == 3 ]]; then
                      printf '::error::Failed to update %s after three attempts\n' "$package"
                      exit 1
                    fi
                    printf '::warning::Update for %s failed; retrying in 10 seconds\n' "$package"
                    sleep 10
                  done

                  git diff --binary -- pkgs/ > "$RUNNER_TEMP/package-after.patch"
                  if ! cmp -s "$RUNNER_TEMP/package-before.patch" "$RUNNER_TEMP/package-after.patch"; then
                    nix build --accept-flake-config --no-link --print-build-logs ".#$package"
                    new_version=$(nix eval --raw ".#packages.x86_64-linux.$package" --apply 'p: p.version or "local"')
                    if [[ "$old_version" == "$new_version" ]]; then
                      printf -- "- %s: updated sources/dependencies (built successfully)\n" "$package" >> "$PR_BODY"
                    else
                      printf -- "- %s: %s -> %s\n" "$package" "$old_version" "$new_version" >> "$PR_BODY"
                    fi
                  fi
                done

                if ! git diff --quiet -- flake.lock; then
                  ${saveOldLock}
                  printf '\n' >> "$PR_BODY"
                  ${flakeDiff} >> "$PR_BODY"
                fi
                cat "$PR_BODY" >> "$GITHUB_STEP_SUMMARY"
                SCRIPT
              '';
            }
            {
              name = "Stage package updates";
              run = ''
                mkdir -p "$RUNNER_TEMP/package-updates"
                git diff --binary -- pkgs/ flake.lock > "$RUNNER_TEMP/package-updates/updates.patch"
                cp "$RUNNER_TEMP/package-updates.md" "$RUNNER_TEMP/package-updates/package-updates.md"
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
                run = ''
                  patch="$RUNNER_TEMP/package-updates/updates.patch"
                  if [[ -s "$patch" ]]; then
                    git apply "$patch"
                  fi
                '';
              }
              (blocks.pr {
                name = "Open combined update PR";
                branch = "updates/custom-packages";
                delete-branch = true;
                commit-message = "chore(deps): update custom packages";
                title = "chore(deps): update custom packages";
                body-path = "\${{ runner.temp }}/package-updates/package-updates.md";
                add-paths = ''
                  pkgs/
                  flake.lock
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
              name = "Summarize flake update";
              env.PACKAGES = "\${{ inputs.package }}";
              run = ''
                mkdir -p "$RUNNER_TEMP/flake-update"
                cp flake.lock "$RUNNER_TEMP/flake-update/flake.lock"
                ${saveOldLock}
                {
                  printf 'Flake inputs updated by the manual workflow. Built: `%s`\n\n' "$PACKAGES"
                  ${flakeDiff}
                } > "$RUNNER_TEMP/flake-update/body.md"
                cat "$RUNNER_TEMP/flake-update/body.md" >> "$GITHUB_STEP_SUMMARY"
              '';
            }
            {
              name = "Upload updated flake.lock";
              uses = "actions/upload-artifact@ea165f8d65b6e75b540449e92b4886f43607fa02"; # v4
              "with" = {
                name = "manual-flake-lock";
                path = "\${{ runner.temp }}/flake-update";
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
                "with" = {
                  name = "manual-flake-lock";
                  path = "\${{ runner.temp }}/flake-update";
                };
              }
              {
                name = "Apply updated flake.lock";
                run = ''cp "$RUNNER_TEMP/flake-update/flake.lock" flake.lock'';
              }
              (blocks.pr {
                name = "Open flake update PR";
                add-paths = "flake.lock";
                branch = "updates/manual-flake-inputs";
                commit-message = "chore(deps): update flake inputs";
                delete-branch = true;
                title = "chore(deps): update flake inputs";
                body-path = "\${{ runner.temp }}/flake-update/body.md";
              })
            ];
          };
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
              "with" = {
                name = "flake-lock";
                path = "\${{ runner.temp }}/flake-update";
              };
            }
            {
              name = "Prefetch displaylink";
              run = ''
                nix develop --accept-flake-config -c just prefetch
              '';
            }
            (buildClosures "old")
            {
              name = "Use updated flake.lock";
              run = ''cp "$RUNNER_TEMP/flake-update/flake.lock" flake.lock'';
            }
            (buildClosures "new")
            {
              name = "Summarize flake update";
              run = ''
                ${saveOldLock}
                {
                  printf 'Automated flake input update. The configured package and host builds passed.\n\n'
                  ${flakeDiff} "$RUNNER_TEMP/closures"
                } > "$RUNNER_TEMP/flake-update/body.md"
                cat "$RUNNER_TEMP/flake-update/body.md" >> "$GITHUB_STEP_SUMMARY"
              '';
            }
            {
              name = "Upload verified flake.lock";
              uses = "actions/upload-artifact@ea165f8d65b6e75b540449e92b4886f43607fa02"; # v4
              "with" = {
                name = "verified-flake-lock";
                path = "\${{ runner.temp }}/flake-update";
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
                "with" = {
                  name = "verified-flake-lock";
                  path = "\${{ runner.temp }}/flake-update";
                };
              }
              {
                name = "Apply verified flake.lock";
                run = ''cp "$RUNNER_TEMP/flake-update/flake.lock" flake.lock'';
              }
              (blocks.pr {
                name = "Open flake update PR";
                add-paths = "flake.lock";
                branch = "updates/flake-lock";
                commit-message = "chore(deps): bump flake.lock";
                delete-branch = true;
                title = "chore(deps): bump flake.lock";
                body-path = "\${{ runner.temp }}/flake-update/body.md";
              })
            ];
          };
        };
      };
  };
}
