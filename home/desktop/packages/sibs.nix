{ pkgs ? import <nixpkgs> {}, ... }: pkgs.stdenv.mkDerivation rec {
  name = "sibs";
  src = pkgs.fetchgit {
    url = "https://repo.dec05eba.com/sibs";
    fetchSubmodules = true;
    rev = "008982a234f0a5ea7d1021dc151278dd143e669f";
    hash = "sha256-8o3GO6Ajb4nvYb6LTEwraCDBaN8nEgSzpMagGWnqc0M=";
  };

  nativeBuildInputs = with pkgs; [ cmake ninja ];
  buildInputs = with pkgs; [ curl libarchive pkg-config ];

  configurePhase = ''
    cmake -G Ninja -DCMAKE_BUILD_TYPE=Release .
  '';

  installPhase = ''
    mkdir --parents "$out/bin/"
    install -Dm755 sibs "$out/bin/sibs"
  '';
}
