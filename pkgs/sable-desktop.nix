{
  fetchurl,
  appimageTools,
}:
appimageTools.wrapType2 (finalAttrs: {
  pname = "sable";
  version = "2.0.3";

  src = fetchurl {
    url = "https://git.sable.moe/SableClient/sable-next/releases/download/v${finalAttrs.version}/sable-${finalAttrs.version}-linux-x86_64.AppImage";
    hash = "sha256-lA1cNLU2UftCy2K9h+j9kZXy4Q2E82Fe2MfTp8XAUjo=";
  };
})
