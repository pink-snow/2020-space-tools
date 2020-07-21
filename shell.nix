let nixpkgsurl =
  https://github.com/NixOS/nixpkgs/archive/5cb5ccb54229efd9a4cd1fccc0f43e0bbed81c5d.tar.gz
  ;
in with import (fetchTarball nixpkgsurl) {};
mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (p: with p; [JuicyPixels JuicyPixels-util errors extra groupBy]))
    (python38.withPackages (p: with p; [pillow]))
    imagemagick

    git
    wget
    unzip

    zsh
  ];
}
