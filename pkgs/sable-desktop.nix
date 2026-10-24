{
  fetchurl,
  appimageTools,
}:
appimageTools.wrapType2 (finalAttrs: {
  pname = "sable";
  version = "2.1.6";

  src = fetchurl {
    url = "https://git.sable.moe/SableClient/sable-next/releases/download/v${finalAttrs.version}/sable-${finalAttrs.version}-x86_64.AppImage";
    hash = "sha256-5+eDWgfJuxFmAAdfsTnlSS5zf+44r1tCIydaUgJ+reg=";
  };
})
