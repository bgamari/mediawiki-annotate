{ mkDerivation, base, miso, stdenv, trec-car-tools, aeson }:
mkDerivation {
  pname = "app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base miso trec-car-tools aeson ];
  description = "First miso app";
  license = stdenv.lib.licenses.bsd3;
}
