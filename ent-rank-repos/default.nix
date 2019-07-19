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

  localDir = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  simplirNix = import ./simplir { inherit nixpkgs; };

  haskellOverrides = self: super:
    let
      trecCarPackages = {
        trec-car-types       = dontCheck (self.callCabal2nix "trec-car-types" (localDir ./trec-car-tools-haskell/trec-car-types) {});
        trec-car-tools       = dontCheck (self.callCabal2nix "trec-car-tools" (localDir ./trec-car-tools-haskell/trec-car-tools) {});
        graph-algorithms     = self.callCabal2nix "graph-algorithms" (localDir ./graph-algorithms) {};
        ent-rank             = self.callCabal2nix "ent-rank" (localDir ./ent-rank) {};
        ent-rank-tools       = self.callCabal2nix "ent-rank-tools" (localDir ./ent-rank-tools) {};

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

        servant = self.callHackage "servant" "0.16" {};
        servant-client = self.callHackage "servant-client" "0.16" {};
        servant-client-core = self.callHackage "servant-client-core" "0.16" {};
        servant-server = self.callHackage "servant-server" "0.16" {};
      };
    in trecCarPackages // { inherit trecCarPackages; };

  haskellPackages = nixpkgs.haskell.packages.ghc864.override {
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

