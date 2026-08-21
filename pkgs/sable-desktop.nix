{
  fetchurl,
  appimageTools,
}:
appimageTools.wrapType2 (finalAttrs: {
  pname = "sable";
  version = "1.21.0";

  src = fetchurl {
    url = "https://github.com/SableClient/Sable/releases/download/v${finalAttrs.version}/Sable-${finalAttrs.version}-linux-x86_64.AppImage";
    hash = "sha256-dAQErvyoZ9rfrvpgdKVLcr0isEN+B92LE6J0wB99xhY=";
  };
})
