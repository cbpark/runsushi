{ mkDerivation, attoparsec, base, bytestring, containers, directory
, double-conversion, filepath, h2decays, hashable, hpack
, optparse-generic, pipes, process, random, stdenv, text
, transformers, vector
}:
mkDerivation {
  pname = "runsushi";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    attoparsec base bytestring containers directory double-conversion
    filepath h2decays hashable optparse-generic pipes process random
    text transformers vector
  ];
  prePatch = "hpack";
  homepage = "https://github.com/cbpark/runsushi#readme";
  description = "Processing inputs and outputs of SusHi";
  license = stdenv.lib.licenses.bsd3;
}
