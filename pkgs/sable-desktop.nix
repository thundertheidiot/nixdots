{
  fetchurl,
  appimageTools,
}:
appimageTools.wrapType2 (finalAttrs: {
  pname = "sable";
  version = "2.3.5";

  src = fetchurl {
    url = "https://git.sable.moe/SableClient/sable-next/releases/download/v${finalAttrs.version}/sable-${finalAttrs.version}-x86_64.AppImage";
    hash = "sha256-NZoXBmOyaQAWtQUmCH0JEqX7ZT/sxzHMuXPSEkOA9uU=";
  };
})
