{ mkDerivation, base, miso, stdenv, trec-car-types, aeson }:
mkDerivation {
  pname = "app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base miso trec-car-types aeson ];
  description = "First miso app";
  license = stdenv.lib.licenses.bsd3;
}
