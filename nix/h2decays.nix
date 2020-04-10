{ mkDerivation, attoparsec, base, blaze-builder, bytestring
, double-conversion, fetchgit, gsl, hashable, hmatrix-gsl
, hpack, lhapdf, optparse-generic, pipes, stdenv, vector
}:
mkDerivation {
  pname = "h2decays";
  version = "0.0.0.0";
  src = fetchgit {
    url = "git@github.com:cbpark/h2decays";
    sha256 = "021zhzhyrama1w3qs10nj3cyfz5knzhx0wr1c3sgmywa4acj071s";
    rev = "f27cdf4c8adb4dd39ba7d869d3290480c5fb8dfe";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base blaze-builder bytestring double-conversion hashable
    hmatrix-gsl pipes vector
  ];
  librarySystemDepends = [ gsl lhapdf ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring optparse-generic pipes vector
  ];
  prePatch = "hpack";
  homepage = "https://github.com/cbpark/h2decays#readme";
  description = "Calculating the branching ratios of heavy Higgs bosons in the 2HDM";
  license = stdenv.lib.licenses.gpl3;
}
