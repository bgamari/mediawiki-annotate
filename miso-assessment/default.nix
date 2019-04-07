{ pkgs ? (import ../simplir/nixpkgs.nix {}) }:

let
  result = import (pkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    sha256 = "1wvdizaq81a50jd121qlk47hnix0q0r1pnq2jqkwyy5ssfq6hpb6";
    rev = "8b5249b966f1406badbada3feebcfbbeab8afa87";
  }) {};

  haskellPackages = pkgs.haskell.packages.ghcjs.override {
    overrides = self: super: let lib = pkgs.haskell.lib; in {
      trec-car-types = haskellPackages.callCabal2nix "trec-car-types" ../trec-car-types { };
      mediawiki-parser = lib.dontCheck (haskellPackages.callCabal2nix "mediawiki-parser" ../mediawiki-parser { tasty-silver = null; });

      doctest = null;
      temporary = lib.dontCheck super.temporary;
      ListLike = lib.dontCheck super.ListLike;
      QuickCheck = lib.dontCheck super.QuickCheck;
      comonad = lib.dontCheck super.comonad;
      half = lib.dontCheck super.half;
      lens = lib.dontCheck super.lens;
      semigroupoids = lib.dontCheck super.semigroupoids;
      tasty-quickcheck = lib.dontCheck super.tasty-quickcheck;
      scientific = lib.dontCheck super.scientific;
      aeson = lib.dontCheck super.aeson;
      cryptohash-sha1 = lib.dontCheck super.cryptohash-sha1;
      text-short = lib.dontCheck super.text-short;
      cborg = lib.dontCheck super.cborg;
      serialise = lib.dontCheck super.serialise;
      http-types = lib.dontCheck super.http-types;
      http-media = lib.dontCheck super.http-media;
      servant = lib.dontCheck super.servant;
      jsaddle = self.callHackage "jsaddle" "0.9.6.0" {};
    };
  };

  app = haskellPackages.callCabal2nix "app" ./. { };
in 
  pkgs.stdenv.mkDerivation {
    name = "app2";
    src = app;
    installPhase = ''
      mkdir $out
      cp -R bin/app.jsexe/*.js $out
      cp ${./index.html} $out/index.html
    '';
  }
