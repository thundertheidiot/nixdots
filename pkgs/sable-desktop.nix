{
  fetchurl,
  appimageTools,
}:
appimageTools.wrapType2 (finalAttrs: {
  pname = "sable";
  version = "26.11.1";

  src = fetchurl {
    url = "https://git.sable.moe/SableClient/sable-next/releases/download/v${finalAttrs.version}/sable-${finalAttrs.version}-x86_64.AppImage";
    hash = "sha256-heKijGeO532W87VPsLzIqP4X/QG7bAd36hXkDCuq9zw=";
  };
})
