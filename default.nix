{ nixpkgs ? (import <nixpkgs> {}) }:

let
  inherit (nixpkgs.haskell.lib) dontCheck doJailbreak;
  inherit (nixpkgs.stdenv) lib;

  cabalFilter = path: type:
    let pathBaseName = baseNameOf path;
    in !(lib.hasPrefix "dist-newstyle" pathBaseName) &&
       !(lib.hasPrefix ".git" pathBaseName) &&
       !(lib.hasPrefix "dist" pathBaseName);

  localDir = builtins.filterSource cabalFilter;

  haskellPackages = nixpkgs.haskell.packages.ghc821.override {
    overrides = self: super:
      let
        localPkgs = {
          trec-car-tools       = dontCheck (self.callCabal2nix "trec-car-tools" (localDir ./.) {});
          mediawiki-parser     = self.callCabal2nix "mediawiki-parser" (localDir ./mediawiki-parser) {};
          mediawiki-import     = self.callCabal2nix "mediawiki-import" (localDir ./mediawiki-import) {};
          car-baselines        = self.callCabal2nix "car-baselines" (localDir ./car-baselines) {};
          filter-duplicates    = self.callCabal2nix "filter-duplicates" (localDir ./filter-duplicates) {};
          assessment-interface = self.callCabal2nix "assessment-interface" (localDir ./assessment-interface) {};
          annotate-server      = self.callCabal2nix "annotate-server" (localDir ./assessment-interface/annotation/server) {};

          simplir              = self.callCabal2nix "simplir" (localDir ./simplir) {};
          simplir-data-source  = self.callCabal2nix "simplir-data-source" (localDir ./simplir/simplir-data-source) {};
          simplir-html-clean   = self.callCabal2nix "simplir-html-clean" (localDir ./simplir/simplir-html-clean) {};
          simplir-trec         = self.callCabal2nix "simplir-trec" (localDir ./simplir/simplir-trec) {};
          simplir-galago       = self.callCabal2nix "simplir-galago" (localDir ./simplir/simplir-galago) {};
          simplir-tools        = self.callCabal2nix "simplir-tools" (localDir ./simplir/simplir-tools) {};
          simplir-word-embedding = self.callCabal2nix "simplir-word-embedding" (localDir ./simplir/simplir-word-embedding) {};
          simplir-trec-streaming = self.callCabal2nix "simplir-trec-streaming" (localDir ./simplir/simplir-trec-streaming) {};
          http-parsers         = self.callCabal2nix "http-parsers" ./simplir/vendor/http-parsers {};
          indexed-vector       = self.callCabal2nix "indexed-vector" ./simplir/vendor/indexed-vector {};
          fork-map             = self.callCabal2nix "fork-map" ./simplir/vendor/fork-map {};

          lzma = dontCheck super.lzma;
          pipes-zlib = doJailbreak super.pipes-zlib;
          text-icu   = dontCheck super.text-icu;
          intset = self.callCabal2nix "intset" ./vendor/intset {};
          optparse-applicative = self.callHackage "optparse-applicative" "0.14.0.0" {};

          #cborg = self.callHackage "cborg" "0.2.0.0" {};
          #serialise = self.callHackage "serialise" "0.2.0.0" {};
          cborg = self.callCabal2nix "cborg" ./vendor/binary-serialise-cbor/cborg {};
          serialise = self.callCabal2nix "serialise" ./vendor/binary-serialise-cbor/serialise {};
          binary-serialise-cbor = self.callCabal2nix "binary-serialise-cbor" ./vendor/binary-serialise-cbor/binary-serialise-cbor {};
          cborg-json            = self.callCabal2nix "cborg-json" ./vendor/binary-serialise-cbor/cborg-json {};
        };
    in localPkgs // { localPkgs = localPkgs; };
  };
in haskellPackages // {
  env = haskellPackages.ghcWithHoogle (pkgs: builtins.attrValues haskellPackages.localPkgs);
}
