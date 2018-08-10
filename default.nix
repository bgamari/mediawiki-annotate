{ nixpkgs ? (import simplir/nixpkgs.nix {}) }:

let
  inherit (nixpkgs.haskell.lib) dontCheck doJailbreak;
  inherit (nixpkgs.stdenv) lib;

  all-cabal-hashes =
    let rev = "a4f65a465ce4b727a44cabc8073a5b09f058335f";
    in {
      url    = "https://github.com/commercialhaskell/all-cabal-hashes/archive/${rev}.tar.gz";
      sha256 = "16rnyxqmr93ahml0fjfa6hmjpmx8sbpfdr52krd3sd6ic9n5p5ix";
    };

  cabalFilter = path: type:
    let pathBaseName = baseNameOf path;
    in !(lib.hasPrefix "dist-newstyle" pathBaseName) &&
       !(lib.hasPrefix ".git" pathBaseName) &&
       !(lib.hasPrefix ".ghc.environment" pathBaseName) &&
       !(lib.hasPrefix "dist" pathBaseName);

  localDir = builtins.filterSource cabalFilter;

  simplirNix = import ./simplir { inherit nixpkgs; };

  haskellOverrides = self: super:
    let
      trecCarPackages = {
        trec-car-tools       = dontCheck (self.callCabal2nix "trec-car-tools" (localDir ./trec-car-tools) {});
        mediawiki-parser     = self.callCabal2nix "mediawiki-parser" (localDir ./mediawiki-parser) {};
        mediawiki-import     = self.callCabal2nix "mediawiki-import" (localDir ./mediawiki-import) {};
        car-baselines        = self.callCabal2nix "car-baselines" (localDir ./car-baselines) {};
        filter-duplicates    = self.callCabal2nix "filter-duplicates" (localDir ./filter-duplicates) {};
        assessment-interface = self.callCabal2nix "trec-car-annotation-interface" (localDir ./assessment-interface) {};
        annotate-server      = self.callCabal2nix "annotate-server" (localDir ./assessment-interface/annotation/server) {};
        trec-car-graph-expansion = self.callCabal2nix "trec-car-graph-expansion" (localDir ./graph-expansion) {};
        graph-algorithms     = self.callCabal2nix "graph-algorithms" (localDir ./graph-algorithms) {};
        db-export            = self.callCabal2nix "db-export" (localDir ./db-export) {};
        wordnet-export       = nixpkgs.callPackage (import ./wordnet-export) { haskellPackages = self; };
        multilang-car        = self.callCabal2nix "multilang-car" (localDir ./multilang-car) {};
        tqa-import           = self.callCabal2nix "tqa-import" ./tqa-import {};

        intset = self.callCabal2nix "intset" ./vendor/intset {};
      };
    in trecCarPackages // { inherit trecCarPackages; };

  haskellPackages = nixpkgs.haskell.packages.ghc843.override {
    overrides = lib.composeExtensions simplirNix.haskellOverrides haskellOverrides;
  };
in {
  pkgs = nixpkgs;
  inherit (haskellPackages.wordnet-export.passthru) ukb;
  inherit haskellPackages haskellOverrides;
  inherit (haskellPackages) trecCarPackages;
  inherit (simplirNix) simplirPackages trec-eval;
  env = haskellPackages.ghcWithHoogle (pkgs: builtins.attrValues haskellPackages.trecCarPackages ++ builtins.attrValues haskellPackages.simplirPackages);
  binaries = nixpkgs.symlinkJoin {
    name = "trec-car-binaries";
    paths = builtins.attrValues haskellPackages.trecCarPackages ++ builtins.attrValues haskellPackages.simplirPackages;
  };
}
