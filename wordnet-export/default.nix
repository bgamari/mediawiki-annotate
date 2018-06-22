{ fetchFromGitHub, haskellPackages }:

let
  ukb =
    let src = fetchFromGitHub {
        owner = "bgamari";
        repo = "ukb-hs";
        rev = "72cb17e22dd57caf36e11cf14565cf0e67a9a382";
        sha256 = null;
      };
    in import src { inherit haskellPackages; };

  wordnet-parse =
    let src = fetchFromGitHub {
        owner = "bgamari";
        repo = "wordnet-parse";
        rev = "754d855f51735a30f38da5ce4fd5f2ac62a8d180";
        sha256 = null;
      };
    in haskellPackages.callCabal2nix "wordnet-parse" src {};
in
  haskellPackages.callCabal2nix "wordnet-export" ./. {
    inherit ukb wordnet-parse;
  } // { passthru.ukb = ukb; } # yuck

