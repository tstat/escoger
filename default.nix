{ mkDerivation, base, bytestring, criterion, HUnit, mtl, stdenv
, test-framework, test-framework-hunit, unix, vector
, vector-algorithms, vty
}:
mkDerivation {
  pname = "escoger";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring mtl unix vector vector-algorithms vty
  ];
  executableHaskellDepends = [ base bytestring mtl unix vector vty ];
  testHaskellDepends = [
    base HUnit test-framework test-framework-hunit vector
  ];
  benchmarkHaskellDepends = [
    base bytestring criterion unix vector vty
  ];
  description = "Terminal fuzzy selector";
  license = stdenv.lib.licenses.mit;
}
