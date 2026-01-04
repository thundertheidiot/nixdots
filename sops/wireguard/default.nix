let
  shared = {
    wg_psk = {
      sopsFile = ./wg-pskey;
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

  bighome = {
    sops.secrets =
      {
        wg_private = {
          sopsFile = ./bighome-wg-privkey;
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
  pubkeyBigHome = ./bighome-wg-pubkey;
  pubkeyVps = ./vps-wg-pubkey;
}
