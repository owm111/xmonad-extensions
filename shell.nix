{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [ (haskellPackages.ghcWithPackages (h: with h; [
    # libraries
    xmonad xmonad-contrib
    # tools
    cabal-install
  ])) ];
}
