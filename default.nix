let 
  pkgs = import <nixpkgs> { };
in 
  pkgs.haskellPackages.developPackage {
    root = ./.;
    source-overrides = {
      grid = ../grid;
    };
  }
