{
  fetchurl,
  appimageTools,
}:
appimageTools.wrapType2 (finalAttrs: {
  pname = "sable";
  version = "2.0.2";

  src = fetchurl {
    url = "https://git.sable.moe/SableClient/sable-next/releases/download/v${finalAttrs.version}/sable-${finalAttrs.version}-linux-x86_64.AppImage";
    hash = "sha256-IMxEjT3j63V1Wz/wXJalhNLOaP5DlpZPtGfwUg4+rmA=";
  };
})
