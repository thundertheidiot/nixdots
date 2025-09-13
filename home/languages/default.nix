{
  config,
  mlib,
  lib,
  pkgs,
  ...
}: let
  inherit (mlib) mkEnOpt;
  inherit (lib) genAttrs mkMerge mkIf listToAttrs;

  mkLangs = l: genAttrs l (n: mkEnOpt "Enable support for ${n}");

  cfg = config.mHome.lang;
in {
  options.mHome.lang = mkLangs [
    "nix"
    "haskell"
    "rust"
    "lua"
    "c_cxx"
    "python"
    "bash"
    "web"
    "latex"
  ];

  options.mHome.setup = {
    fullLanguages = mkEnOpt "Enable all programming languages";
  };

  config = let
    enAll = list:
      listToAttrs (map (i: {
          name = i;
          value = true;
        })
        list);
  in
    mkMerge [
      (mkIf config.mHome.setup.fullLanguages {
        mHome.lang = enAll [
          "nix"
          "haskell"
          "rust"
          "lua"
          "c_cxx"
          "python"
          "bash"
          "web"
          # "latex"
        ];
      })
      {
        home.packages = with pkgs;
          mkMerge [
            (mkIf cfg.nix [
              nixd # lsp
              alejandra # fmt
            ])
            (mkIf cfg.haskell [
              (haskellPackages.ghcWithPackages (pkgs: []))
              haskell-language-server
            ])
            (mkIf cfg.lua [
              lua-language-server
            ])
            (mkIf cfg.latex [
              texlive.combined.scheme-full
            ])
            (mkIf cfg.c_cxx [
              clang-tools
            ])
            (mkIf cfg.bash [
              nodePackages.bash-language-server
            ])
            (mkIf cfg.python [
              basedpyright
              python313Packages.python
            ])
            (mkIf cfg.web [
              nodejs
              typescript-language-server
              nodePackages.prettier
            ])
          ];
      }
    ];
}
