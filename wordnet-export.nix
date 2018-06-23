let
  trecCar = import ./. {};
in
with trecCar.pkgs;
rec {
  dbName = "enwiki_20161220_v2_0_1_test200";

  lkbSources = stdenv.mkDerivation {
    name = "lkb-sources";
    src = fetchurl {
      url = http://ixa2.si.ehu.es/ukb/lkb_sources.tar.bz2;
      sha256 = "16xmn1ninz23aysx716ngmwn97h2z3v2zkbbkvgm1fz4brhmqfly";
    };
    installPhase = "cp -R . $out";
  };

  wordNet = stdenv.mkDerivation {
    name = "wordnet-db";
    src = fetchurl {
      url = http://wordnetcode.princeton.edu/3.0/WNdb-3.0.tar.gz;
      sha256 = "1j3d0hjmskm4sa8z2ygp1vrmhc81hv0jagmfkcp8rygmj6hip2v5";
    };
    installPhase = "cp -R . $out";
  };

  compiledKb = stdenv.mkDerivation {
    name = "compiled-kb";
    src = lkbSources;
    buildPhase = " ${trecCar.trecCarPackages.wordnet-export.passthru.ukb.passthru.ukb}/bin/compile_kb -o wn30.bin ${lkbSources}/30/wnet30_rels.txt ";
    installPhase = " mkdir $out; cp wn30.bin $out ";
  };

  script = writeScript "build.sh" ''
    #!/bin/sh

    set -e

    pages=$1

    psql <<EOF
    DROP DATABASE ${dbName};
    CREATE DATABASE ${dbName};
    EOF

    echo "Exporting pages..."
    ${trecCar.trecCarPackages.db-export}/bin/db-export -c postgres:///${dbName} $pages +RTS -N$CORES

    echo "Exporting wordnet..."
    ${trecCar.trecCarPackages.wordnet-export}/bin/wordnet-export -c postgres:///${dbName} -W ${wordNet} -D ${lkbSources}/30/wnet30_dict.txt -K ${compiledKb}/wn30.bin -p $pages +RTS -N$CORES
  '';
}