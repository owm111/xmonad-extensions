xmonad-extensions
=================

My extensions for xmonad.

Usage with NixOS flakes
-----------------------

`flake.nix` includes a NixOS module which adds xmonad-extensions to the global `haskell` package set, so it can easily be added to xmonad via `services.xserver.windowManagers.xmonad.extraPackages`.

Basic example of using xmonad-extensions with NixOS configured via a flake:

```nix
{
    description = "My NixOS config";

    # other inputs...
    inputs.xmonad-extensions.url = "github:owm111/xmonad-extensions";

    output = { self, nixpkgs, /* other inputs..., */ xmonad-extensions, ... }: {
        # other outputs...
        nixosConfigurations.mysystem = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
                # other modules...
                xmonad-extensions.nixosModule
                ({ ... }: {
                    services.xserver.windowManager.xmonad.extraPackages = p: [
                        xmonad-extensions
                    ];
                })
            ];
        };
    };
}
```
