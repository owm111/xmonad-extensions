final: prev: {
  haskell = prev.haskell // {
    packageOverrides = final.lib.composeExtensions prev.haskell.packageOverrides
      (self: super: {
        xmonad-extensions = self.callCabal2nix "xmonad-extensions" ./. {};
      });
  };
}
