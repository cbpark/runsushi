{ mkDerivation, attoparsec, base, blaze-builder, bytestring
, double-conversion, fetchgit, gsl, hashable, hmatrix-gsl
, hpack, lhapdf, optparse-generic, pipes, stdenv, vector
}:
mkDerivation {
  pname = "h2decays";
  version = "0.0.0.0";
  src = fetchgit {
    url = "git@github.com:cbpark/h2decays";
    sha256 = "00vqmbq8v087gm0hhdzbal4c2x0x7glra5vmsrhxgmvpcq6wqb36";
    rev = "5937c5c895046fe43396d11d925a64e78309e749";
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
