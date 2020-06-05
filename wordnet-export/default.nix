{ fetchFromGitHub, haskellPackages }:

let
  ukb =
    let src = fetchFromGitHub {
        owner = "bgamari";
        repo = "ukb-hs";
        rev = "4605b1d1b8acd2232b58bbe9fd970a0437432795";
        sha256 = "0c3qnz7s80zf80v5k0x14yd2qmyp8s2919ds560jm1jcwzf9nzsx";
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

