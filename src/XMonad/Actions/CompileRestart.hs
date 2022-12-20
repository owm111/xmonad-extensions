{-|
Taken from the example on the services.xserver.windowManger.xmonad.config
section of NixOS' configuration.nix(5) man page.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module XMonad.Actions.CompileRestart
    ( compileRestart
    , launch'
    , compileRestartKeys
    ) where

import Control.Monad
import Data.Map.Strict qualified as M
import System.Environment
import System.FilePath
import System.Info
import System.Posix.Process
import XMonad
import XMonad.Util.Keymap

compiledConfig = "xmonad-" ++ arch ++ "-" ++ os

{-|
Compile xmonad from @$HOME/.xmonad@ and restart using it.
-}
compileRestart resume = do
    dirs <- asks directories
    whenX (recompile dirs True) $ do
        when resume writeStateToFile
        catchIO $ do
            args <- getArgs
            executeFile (cacheDir dirs </> compiledConfig) False args Nothing

{-|
'launch' à la xmonad-0.15.
-}
launch' :: (LayoutClass l Window, Read (l Window)) => XConfig l -> IO ()
launch' config = getDirectories >>= launch config

{-|
Generate a keymap with mappings to restart the system XMonad (which
presumably is statically configured and launching with 'launch'')
and to recompile with the configuration in @$HOME/.xmonad@.
-}
compileRestartKeys ::
    {-| The base mask for the binds of this map. The recompile bind will
    use this plus control. -}
    KeyMaskFn ->
    {-| The key to bind to. -}
    KeySym ->
    Keymap
compileRestartKeys maskf key XConfig {modMask} = M.fromList
    [ ((maskf modMask, key), restart "xmonad" True)
    , ((maskf modMask .|. controlMask, key), compileRestart True)
    ]
