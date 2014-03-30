with import <nixos-unstable> {};
stdenv.mkDerivation rec {
  name = "env";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    ghc
    cabal-install
  ];
}
