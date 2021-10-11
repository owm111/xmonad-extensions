{
  description = "My extensions for xmonad.";

  outputs = { self }: {
    overlay = import ./overlay.nix;    
    nixosModule = { ... }: {
      nixpkgs.overlays = [ self.overlay ];
    };
  };
}
