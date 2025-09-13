let
  shared = {
    wg_home_psk = {
      sopsFile = ./home-wg-pskey;
      format = "binary";
    };

    wg_vps_psk = {
      sopsFile = ./vps-wg-pskey;
      format = "binary";
    };
  };
in {
  home = {
    sops.secrets =
      {
        wg_private = {
          sopsFile = ./home-wg-privkey;
          format = "binary";
        };
      }
      // shared;
  };

  vps = {
    sops.secrets =
      {
        wg_private = {
          sopsFile = ./vps-wg-privkey;
          format = "binary";
        };
      }
      // shared;
  };

  pubkeyHome = ./home-wg-pubkey;
  pubkeyVps = ./vps-wg-pubkey;
}
