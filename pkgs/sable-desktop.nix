{
  fetchurl,
  appimageTools,
}:
appimageTools.wrapType2 (finalAttrs: {
  pname = "sable";
  version = "1.22.11";

  src = fetchurl {
    url = "https://github.com/SableClient/Sable/releases/download/v${finalAttrs.version}/Sable-${finalAttrs.version}-linux-x86_64.AppImage";
    hash = "sha256-fHdYj/JZ2uLHZeey3E3nOrZLyzv4rembxP0VTS9zot0=";
  };
})
