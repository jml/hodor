{ mkDerivation, base, bytestring, containers, directory, hspec, mtl
, optparse-applicative, parsec, QuickCheck, rainbow, regex-compat
, stdenv, text, time, transformers, unix, vector, yaml
}:
mkDerivation {
  pname = "hodor";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base bytestring containers directory mtl optparse-applicative
    parsec rainbow regex-compat text time transformers unix vector yaml
  ];
  testDepends = [ base containers hspec mtl parsec QuickCheck time ];
  license = stdenv.lib.licenses.asl20;
}
