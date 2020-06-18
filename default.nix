{ compiler ? "ghc8101" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "som" =
        hself.callCabal2nix
          "som"
          (gitignore ./.)
          {};
      ### local imports
      "grid" =
        (import /home/amy/github/grid/default.nix {}).grid;
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."som"
    ];
    buildInputs = with pkgs.haskellPackages; [
      myHaskellPackages.cabal-install
      ghcid
      ormolu
      hlint
      (import sources.niv {}).niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = true;
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."som");

  docker = pkgs.dockerTools.buildImage {
    name = "som";
    config.Cmd = [ "${exe}/bin/som" ];
  };
in
{
  inherit shell;
  inherit exe;
  inherit docker;
  inherit myHaskellPackages;
  "som" = myHaskellPackages."som";
}
