{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, deepseq, grid, QuickCheck
      , random, stdenv, test-framework, test-framework-quickcheck2
      }:
      mkDerivation {
        pname = "som";
        version = "10.1.8";
        src = ./.;
        libraryHaskellDepends = [ base containers deepseq grid ];
        testHaskellDepends = [
          base containers deepseq grid QuickCheck random test-framework
          test-framework-quickcheck2
        ];
        homepage = "https://github.com/mhwombat/som#readme";
        description = "Self-Organising Maps";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
