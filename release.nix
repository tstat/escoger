{ compiler ? "ghc822" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              project1 =
                haskellPackagesNew.callPackage ./default.nix { };
            };
          };
        };
      };
    };
  };
  pkgs = import <nixos-unstable> { inherit config; };

in
  { project1 = pkgs.haskell.packages.${compiler}.project1;
  }
