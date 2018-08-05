{ fetchFromGitHub, haskellPackages }:

let
  ukb =
    let src = fetchFromGitHub {
        owner = "bgamari";
        repo = "ukb-hs";
        rev = "fc789861c0e0340a6d91efa5871fee29d4909788";
        sha256 = null;
      };
    in import src { inherit haskellPackages; };

  wordnet-parse =
    let src = fetchFromGitHub {
        owner = "bgamari";
        repo = "wordnet-parse";
        rev = "96f90c1e2d9924f3c65fc454bb9237ca38b83dd0";
        sha256 = null;
      };
    in haskellPackages.callCabal2nix "wordnet-parse" src {};
in
  haskellPackages.callCabal2nix "wordnet-export" ./. {
    inherit ukb wordnet-parse;
  } // { passthru.ukb = ukb; } # yuck

