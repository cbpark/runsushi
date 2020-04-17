{ mkDerivation, attoparsec, base, blaze-builder, bytestring
, double-conversion, fetchgit, gsl, hashable, hmatrix-gsl
, hpack, lhapdf, optparse-generic, pipes, stdenv, vector
}:
mkDerivation {
  pname = "h2decays";
  version = "0.0.0.0";
  src = fetchgit {
    url = "git@github.com:cbpark/h2decays";
    sha256 = "0rs0ya2cpf7chb8ljwl41f9zq0afbilf0qd6vna1i6c050yyf8sv";
    rev = "facc1e2a02c802877c7f1c0819695cabbbf78225";
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
