{
  config,
  mlib,
  lib,
  ...
}:
let
  inherit (mlib) mkEnOptTrue;
  inherit (lib) mkIf;

  cfg = config.mHome.git;
  key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBKwHM/9spQfyeNIl/p8N8XBuoKj8UrhuhhlbEwkrgjZ";
in
{
  options = {
    mHome.git.enable = mkEnOptTrue "Configure git.";
  };

  config = mkIf cfg.enable {
    programs.git = {
      enable = true;
      lfs.enable = true;

      signing = {
        key = "${config.home.homeDirectory}/.ssh/id_ed25519.pub";
        format = "ssh";
        signByDefault = true;
      };

      settings = {
        user = {
          name = "Thunder";
          email = "thunder@disroot.org";
        };
        github.user = "thundertheidiot";
        pull.rebase = false;
        color.ui = "auto";
        init.defaultBranch = "main";
        gpg.ssh.allowedSignersFile = "${config.xdg.configHome}/git/allowed_signers";
        http = {
          lowSpeedLimit = 1000;
          lowSpeedTime = 60;
          postBuffer = 524288000;
        };
      };
    };

    xdg.configFile."git/allowed_signers".text = "thunder@disroot.org ${key}\n";
  };
}
