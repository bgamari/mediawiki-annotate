{ nixpkgs ? (import simplir/nixpkgs.nix {}) }:

let
  inherit (nixpkgs.haskell.lib) dontCheck doJailbreak;
  inherit (nixpkgs.stdenv) lib;

  all-cabal-hashes =
    let rev = "7482ee903ffef3c31b57bcdea07d455052557d38";
    in {
      url    = "https://github.com/commercialhaskell/all-cabal-hashes/archive/${rev}.tar.gz";
      sha256 = "26rnyxqmr93ahml0fjfa6hmjpmx8sbpfdr52krd3sd6ic9n5p5ix";
    };

  cabalFilter = path: type:
    let pathBaseName = baseNameOf path;
    in !(lib.hasPrefix "dist-newstyle" pathBaseName) &&
       !(lib.hasPrefix ".git" pathBaseName) &&
       !(lib.hasPrefix ".ghc.environment" pathBaseName) &&
       !(lib.hasPrefix ".iml" pathBaseName) &&
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
        trec-news            = self.callCabal2nix "trec-news" ./trec-news {};
        epfl-section-recommendation = self.callCabal2nix "epfl-section-recommendation" ./epfl-section-recommendation {};
        dbpedia-entity-import= self.callCabal2nix "dbpedia-entity-import" ./dbpedia-entity-import {};
        learning-to-walk     = self.callCabal2nix "learning-to-walk" ./learning-to-walk {};


        intset = self.callCabal2nix "intset" ./vendor/intset {};
        graphviz = self.callCabal2nix "graphviz" (nixpkgs.fetchFromGitHub {
          owner = "bgamari";
          repo = "graphviz";
          rev = "804db2d4805d210c8e160e9654e1e95bd898c077";
          sha256 = "0iq1slrla554b4g29bfx61ak2p3nxfw09nrdbhln0f43hmcpw8d7";
        }) { inherit (nixpkgs) graphviz; };
        hpc-coveralls = self.callCabal2nix "hpc-coveralls" (nixpkgs.fetchFromGitHub {
          owner = "bgamari";
          repo = "hpc-coveralls";
          rev = "a2d500316fecb8ee49c034e2781862c8606b96af";
          sha256 = "17d3ljibsdsxbsqrdjx6rn0ww8ck0lycp2pwfh71ilvwbm5wlbyb";
        }) {};
      };
    in trecCarPackages // { inherit trecCarPackages; };

  haskellPackages = nixpkgs.haskell.packages.ghc863.override {
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
