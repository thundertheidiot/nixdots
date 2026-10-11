{
  fetchurl,
  appimageTools,
}:
appimageTools.wrapType2 (finalAttrs: {
  pname = "sable";
  version = "2.0.0";

  src = fetchurl {
    url = "https://git.sable.moe/SableClient/sable-next/releases/download/v${finalAttrs.version}/sable-${finalAttrs.version}-linux-x86_64.AppImage";
    hash = "sha256-0t0C2Rkv7DPiNL5s9clpAmeiPoKFKkDwfaSGJBPY68U=";
  };
})
