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

  app =
    let
      src = pkgs.nix-gitignore.gitignoreSource ["default.nix" "html"] ./.;
    in haskellPackages.callCabal2nix "app" src { };

  combined = pkgs.stdenv.mkDerivation {
    name = "app-minified";
    nativeBuildInputs = [ pkgs.closurecompiler ];
    src = app;
    buildPhase = ''
      cd bin/app.jsexe
      closure-compiler all.js \
        --externs=all.js.externs > all.min.js
    '';
    installPhase = ''
      mkdir -p $out
      cp all.min.js $out
    '';
  };
in 
  pkgs.stdenv.mkDerivation {
    name = "app2";
    src = combined;
    installPhase = ''
      mkdir $out
      cp all.min.js $out/all.js
      cp ${./html/assess.css} $out/assess.css
      cp ${app}/bin/app.jsexe/runmain.js $out/
      cp ${./html/assess.html} $out/assess.html
      cp -R ${./data} $out/data
      chmod 755 $out/data
    '';
    passthru = {
      env = app.env;
      raw = app;
    };
  }
