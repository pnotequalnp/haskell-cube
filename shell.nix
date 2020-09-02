{ pkgs ? import <nixpkgs> {} }:
let
  hs = pkgs.haskell.packages.ghc884;
  deps = ps: with ps; [
    base
    # ghc-mod
  ];
  haskellEnv = pkgs.haskell.packages.ghc884.ghcWithPackages deps;
in pkgs.mkShell {
  buildInputs = with pkgs; [
    zlib
    hs.cabal-install
    haskellEnv
    hs.haskell-language-server
  ];
}
