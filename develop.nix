# For use with nix-shell
{ nixpkgs ? (import <nixpkgs> {}) }:

with nixpkgs;
let
  trec-eval = callPackage ./simplir/trec-eval.nix {};
in stdenv.mkDerivation {
  name = "hi";
  buildInputs = [ zlib icu icu.out expat lzma trec-eval ];
}
