{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [ (haskellPackages.ghcWithPackages (h: with h; [
    # libraries
    xmonad xmonad-contrib
    data-default X11
    # tools
    cabal-install
  ])) ];
}
