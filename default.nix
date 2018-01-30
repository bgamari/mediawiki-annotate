{ nixpkgs ? (import <nixpkgs> {}) }:

let
  inherit (nixpkgs.haskell.lib) dontCheck doJailbreak;
  inherit (nixpkgs.stdenv) lib;

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
        trec-car-tools       = dontCheck (self.callCabal2nix "mediawiki-annotate" (localDir ./.) {});
        mediawiki-parser     = self.callCabal2nix "mediawiki-parser" (localDir ./mediawiki-parser) {};
        mediawiki-import     = self.callCabal2nix "mediawiki-import" (localDir ./mediawiki-import) {};
        car-baselines        = self.callCabal2nix "car-baselines" (localDir ./car-baselines) {};
        filter-duplicates    = self.callCabal2nix "filter-duplicates" (localDir ./filter-duplicates) {};
        assessment-interface = self.callCabal2nix "trec-car-annotation-interface" (localDir ./assessment-interface) {};
        annotate-server      = self.callCabal2nix "annotate-server" (localDir ./assessment-interface/annotation/server) {};
        trec-car-graph-expansion      = self.callCabal2nix "trec-car-graph-expansion" (localDir ./graph-expansion) {};
        graph-algorithms     = self.callCabal2nix "graph-algorithms" (localDir ./graph-algorithms) {};
        db-export = self.callCabal2nix "db-export" (localDir ./db-export) {};
        multilang-car = self.callCabal2nix "multilang-car" (localDir ./multilang-car) {};

        intset = self.callCabal2nix "intset" ./vendor/intset {};
      };
    in trecCarPackages // { inherit trecCarPackages; };

  haskellPackages = nixpkgs.haskell.packages.ghc822.override {
    overrides = lib.composeExtensions simplirNix.haskellOverrides haskellOverrides;
  };
in {
  inherit haskellPackages haskellOverrides;
  inherit (haskellPackages) trecCarPackages;
  env = haskellPackages.ghcWithHoogle (pkgs: builtins.attrValues haskellPackages.trecCarPackages ++ builtins.attrValues haskellPackages.simplirPackages);
}
