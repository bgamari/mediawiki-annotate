{ pkgs ? (import ../ENT-rank/trec-car-tools-haskell/simplir/nixpkgs.nix {}) }:

let
  misoSrc = pkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    sha256 = "1wvdizaq81a50jd121qlk47hnix0q0r1pnq2jqkwyy5ssfq6hpb6";
    rev = "8b5249b966f1406badbada3feebcfbbeab8afa87";
  };
  result = import misoSrc {};

  haskellPackages = pkgs.haskell.packages.ghcjs.override {
    overrides = self: super: let lib = pkgs.haskell.lib; in {
      trec-car-types = haskellPackages.callCabal2nix "trec-car-types" ../ENT-rank/trec-car-tools-haskell/trec-car-types { };
      # trec-car-tools = haskellPackages.callCabal2nix "trec-car-tools" ../ENT-rank/trec-car-tools-haskell/trec-car-tools { };
      miso-types = haskellPackages.callCabal2nix "miso-types" ../miso-types { };
      tqa-import = haskellPackages.callCabal2nix "tqa-import" ../tqa-import { };
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
      text-short = let
                     src = pkgs.fetchFromGitHub {
                       owner = "bgamari";
                       repo = "text-short";
                       rev = "551d2cbaf9b2e345c9811dad8e637d271bdc91c1";
                       sha256 = "sha256:0z30zalsdjly7qwl4j43iq2z7wg30v9qrwd5b3l5228kl1g9spk4";
                     };
                   in lib.dontCheck (super.callCabal2nix "text-short" src {});
      cborg = lib.dontCheck super.cborg;
      serialise = lib.dontCheck super.serialise;
      http-types = lib.dontCheck super.http-types;
      http-media = lib.dontCheck super.http-media;
      servant = lib.dontCheck super.servant;
      ghcjs-dom = self.callHackage "ghcjs-dom" "0.9.4.0" {};
      ghcjs-dom-jsffi = self.callHackage "ghcjs-dom-jsffi" "0.9.2.0" {};
      jsaddle = self.callHackage "jsaddle" "0.9.6.0" {};
      porter = self.callCabal2nix "porter" ../vendor/porter {};
      jsaddle-warp = super.callPackage "${misoSrc}/jsaddle-warp-ghcjs.nix" {};
    };
  };

  app =
    let
      src = pkgs.nix-gitignore.gitignoreSource ["default.nix" "html"] ./.;
    in haskellPackages.callCabal2nix "app" src { };

  combined = minify "app" app;

  minify = name: src: pkgs.stdenv.mkDerivation {
    name = "app-minified";
    nativeBuildInputs = [ pkgs.closurecompiler ];
    inherit src;
    buildPhase = ''
      for i in bin/*.jsexe; do
        pushd $i
        closure-compiler all.js \
          --externs=all.js.externs > all.min.js
        popd
      done
    '';
    installPhase = ''
      mkdir -p $out
      cp -R * $out
    '';
  };

  # symlinkJoin

  app2 = pkgs.stdenv.mkDerivation {
    name = "app2";
    src = combined;
    installPhase = ''
      mkdir $out
      cp bin/app.jsexe/all.min.js $out/all.js
      cp bin/list.jsexe/all.min.js $out/list.js
      cp bin/list-l.jsexe/all.min.js $out/list-l.js
      cp bin/minimal.jsexe/all.js $out/minimal.js
      cp bin/tqa-new-heading.jsexe/all.js $out/tqa-new-heading.js
      cat >> $out/tqa-new-heading.js <<EOF
      function h\$hs_text_short_is_valid_utf8(a, b, c) {
        return 0;
      }
      EOF
      cp ${./html/assess.css} $out/assess.css
      cp ${app}/bin/app.jsexe/runmain.js $out/
      cp ${./html/assess.html} $out/assess.html
      cp ${./html/list.html} $out/list.html
      cp ${./html/list-l.html} $out/list-l.html
      cp ${./html/minimal.html} $out/minimal.html
      cp ${./html/tqa.html} $out/tqa.html
      cp ${./html/inquery-en.txt} $out/inquery-en.txt
      cp -R ${./data} $out/data
      chmod 755 $out/data
    '';
    passthru = {
      env = app.env;
      raw = app;
    };
  };
in
  if pkgs.lib.inNixShell
  then app2.env
  else app2
