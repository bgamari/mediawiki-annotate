# For use with nix-shell
{ nixpkgs ? (import ./ENT-rank/trec-car-tools-haskell/simplir/nixpkgs.nix {}) }:

with nixpkgs;
let
  trec-eval = callPackage ./ENT-rank/trec-car-tools-haskell/simplir/trec-eval.nix {};
in stdenv.mkDerivation {
  name = "mediawiki-annotate-develop";
  buildInputs = [
    curl binutils-unwrapped leveldb
    zlib icu icu.out expat lzma trec-eval kyotocabinet
    postgresql ghc cabal-install
  ];
}
