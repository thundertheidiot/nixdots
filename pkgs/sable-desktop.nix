{
  fetchurl,
  appimageTools,
}:
appimageTools.wrapType2 (finalAttrs: {
  pname = "sable";
  version = "26.11.3";

  src = fetchurl {
    url = "https://git.sable.moe/SableClient/sable-next/releases/download/v${finalAttrs.version}/sable-${finalAttrs.version}-x86_64.AppImage";
    hash = "sha256-Iyt53NjUwZUZeegfzPDFEqMbzNf4mSFCpnaaNqFNjcA=";
  };
})
