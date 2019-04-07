{ pkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "a0aeb23";
    sha256 = "04dgg0f2839c1kvlhc45hcksmjzr8a22q1bgfnrx71935ilxl33d";
  }){}
}:
let
  trec-car-tools = pkgs.haskell.packages.ghcjs.callCabal2nix "trec-car-tools" ../trec-car-tools { inherit mediawiki-parser simplir;};
  http-parsers = pkgs.haskell.packages.ghcjs.callCabal2nix "http-parsers" ../simplir/vendor/http-parsers {};
  indexed-vector = pkgs.haskell.packages.ghcjs.callCabal2nix "indexed-vector" ../simplir/vendor/indexed-vector {};
  mediawiki-parser = pkgs.haskell.packages.ghcjs.callCabal2nix "mediawiki-parser" ../mediawiki-parser {};
  simplir-html-clean = pkgs.haskell.packages.ghcjs.callCabal2nix "simplir-html-clean" ../simplir/simplir-html-clean {};
  simplir = pkgs.haskell.packages.ghcjs.callCabal2nix "simplir" ../simplir/simplir { inherit http-parsers indexed-vector simplir-html-clean; };


  result = import (pkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    sha256 = "1wvdizaq81a50jd121qlk47hnix0q0r1pnq2jqkwyy5ssfq6hpb6";
    rev = "8b5249b966f1406badbada3feebcfbbeab8afa87";
  }) {};
in pkgs.haskell.packages.ghcjs.callPackage ./app.nix {
  miso = result.miso-ghcjs;
  inherit trec-car-tools;
}
