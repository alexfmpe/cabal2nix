{ pkgs ? import (builtins.fetchTarball {
    name = "release-24.05";
    url = https://github.com/nixos/nixpkgs/archive/3e63890c62a81c96a4ce3c48fdc55c16f75c385e.tar.gz;
    sha256 = "sha256:0gmmgxvhd14qfjp81q5kffmr31s7nmj3adxc678rxm6d18dxpdq2";
  }) {}
, ghcVersion ? pkgs.haskellPackages.ghc.version
, withHls ? true
}:

let
  haskellPackages = pkgs.haskell.packages."ghc${
    builtins.replaceStrings [ "." ] [ "" ] ghcVersion
  }";
  ghc = haskellPackages.ghcWithHoogle (hps: [
    hps.ansi-wl-pprint
    hps.hopenssl
    hps.hpack
    hps.lens
    hps.optparse-applicative
    hps.pretty
    hps.split
    hps.yaml
    hps.monad-par
    hps.monad-par-extras
    hps.tasty
    hps.tasty-golden
    hps.utf8-string
    hps.tar
    hps.hspec
    hps.parsec-class
  ]);

in pkgs.mkShell {
  packages = [
    ghc
    pkgs.cabal-install
    pkgs.ghcid
    pkgs.haskell-ci
    (pkgs.lib.getLib pkgs.openssl)
  ] ++ pkgs.lib.optionals withHls [
    haskellPackages.haskell-language-server
  ];

  # Make Paths_ module of distribution-nixpkgs find its data files in the shell.
  # https://cabal.readthedocs.io/en/latest/cabal-package.html#accessing-data-files-from-package-code
  distribution_nixpkgs_datadir = toString ./distribution-nixpkgs;
}
