{ fetchFromGitHub, haskellPackages }:

let
  ukb =
    let src = fetchFromGitHub {
        owner = "bgamari";
        repo = "ukb-hs";
        rev = "c63896a592b8063ff589648ea9e24736f777cf02";
        sha256 = "17v2c3mbv02lvsskkd75dihwf0difk1ln3zs04ilps2dbnifzf3k";
      };
    in import src { inherit haskellPackages; };

  wordnet-parse =
    let src = fetchFromGitHub {
        owner = "bgamari";
        repo = "wordnet-parse";
        rev = "3c530a48cdb9eb003f27486839543427bba08896";
        sha256 = "1h2kcp210k2hnpscjhywi5lk7f9v2zgkzyh33j7f3qgldqchjkz7";
      };
    in haskellPackages.callCabal2nix "wordnet-parse" src {};
in
  haskellPackages.callCabal2nix "wordnet-export" ./. {
    inherit ukb wordnet-parse;
  } // { passthru.ukb = ukb; } # yuck

