{ mkDerivation, base, containers, deepseq, grid, QuickCheck, random
, stdenv, test-framework, test-framework-quickcheck2
}:
mkDerivation {
  pname = "som";
  version = "10.1.11";
  src = ./.;
  libraryHaskellDepends = [ base containers deepseq grid ];
  testHaskellDepends = [
    base containers deepseq grid QuickCheck random test-framework
    test-framework-quickcheck2
  ];
  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
  homepage = "https://github.com/mhwombat/som";
  description = "Self-Organising Maps";
  license = stdenv.lib.licenses.bsd3;
}
