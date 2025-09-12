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
      }) ["framework" "uwu"];

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
          "with".hatchet-protocol = "rampage";
        };

        nixInstaller = {
          name = "Lix Installer";
          env = {
            NIX_INSTALLER_EXTRA_CONF = ''
              trusted-users = root runner
              access-tokens = github.com=''${{ secrets.GITHUB_TOKEN }}
            '';
          };
          run = "curl --proto '=https' --tlsv1.2 -sSf -L https://install.lix.systems/lix | sh -s -- install --no-confirm";
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
              cron = "0 03 */2 * *";
            }
          ];
          workflow_dispatch = {};
        };

        jobs.update = mkBasicNix [
          {
            name = "Update Flake Inputs";
            run = "nix flake update --accept-flake-config";
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
