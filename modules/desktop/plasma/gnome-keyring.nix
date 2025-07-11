{config, ...}: let
  inherit (config.flake.lib.plasma) F;
in {
  flake.modules.nixos.plasma = {
    services.gnome.gcr-ssh-agent.enable = false;
    programs.ssh = {
      startAgent = true;
      enableAskPassword = true;
    };

    xdg.portal.config = {
      kde = {
        default = ["kde"];
        "org.freedesktop.impl.portal.Secret" = ["gnome-keyring" "kwallet"];
        "org.freedesktop.impl.portal.Settings" = ["kde" "gtk"];
      };
    };

    environment.variables = {
      SSH_ASKPASS_REQUIRE = "prefer";
    };
  };

  flake.modules.homeManager.plasma = {
    # Backwards compatibility, these were previously forced to false, this will upgrade old configurations
    programs.plasma.configFile."kwalletrc" = {
      Wallet.Enabled = F true;
      "org.freedesktop.secrets"."apiEnabled" = F true;
    };
  };
}
