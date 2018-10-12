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
        rev = "96f90c1e2d9924f3c65fc454bb9237ca38b83dd0";
        sha256 = "0877219fwdccn75xizfgc112lf4ygx5xflp1drvwrlxrsiyr7hn7";
      };
    in haskellPackages.callCabal2nix "wordnet-parse" src {};
in
  haskellPackages.callCabal2nix "wordnet-export" ./. {
    inherit ukb wordnet-parse;
  } // { passthru.ukb = ukb; } # yuck

