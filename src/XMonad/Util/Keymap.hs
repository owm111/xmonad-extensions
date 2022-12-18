{-|
This module provides a convient synomym for the type of 'keys' and some
functions to generate keymaps.

Note that keymaps are monoids, so they can be combined with 'mconcat',
'<>', etc.
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module XMonad.Util.Keymap where

import Data.Map qualified as M
import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.StackSet qualified as W

{-|
Synonmym for the type of 'keys'.
-}
type Keymap = XConfig Layout -> M.Map (KeyMask, KeySym) (X ())

{-|
A function that to determine a 'KeyMask', given 'XConfig'\'s 'modMask'.

For just the 'modMask', use @id@.  For 'modMask' with another mask,
use @(.|. mask)@. For something that does involve the 'modMask', use
@(const mask)@.
-}
type KeyMaskFn = KeyMask -> KeyMask

{-|
Generate a keymap like the default workspace greedy view and shift
bindings.
-}
workspaceKeys ::
    {-| The base mask for the binds of this map. Greedy view bindings
    will use just the base mask, and shift bindings will use the base
    mask with shift. -}
    KeyMaskFn ->
    {-| List of keys to use. These will be zipped with the configuration's
    workspaces. -}
    [KeySym] ->
    Keymap
workspaceKeys maskf keys XConfig {modMask, workspaces} = M.fromList $ do
    (w, k) <- zip workspaces keys
    (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    pure ((maskf modMask .|. m, k), windows (f w))

{-|
Generate a keymap like the default screen view and shift bindings.
-}
screenKeys ::
    {-| The base mask for the binds of this map. View bindings will use
    the base mask, and shift will use the base mask plus shift. -}
    KeyMaskFn ->
    {-| List of keys to use. These will be zipped with @[0 ..]@. -}
    [KeySym] ->
    Keymap
screenKeys maskf keys XConfig {modMask} = M.fromList $ do
    (n, k) <- zip [0 ..] keys
    (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    pure ((maskf modMask .|. m, k), screenWorkspace n >>= mapM_ (windows . f))

{-|
Like 'screenKeys', but uses "XMonad.Actions.PhysicalScreens".
-}
physicalScreenKeys :: KeyMaskFn -> [KeySym] -> Keymap
physicalScreenKeys maskf keys XConfig {modMask} = M.fromList $ do
    (n, k) <- zip [0 ..] keys
    (f, m) <- [(viewScreen, 0), (sendToScreen, shiftMask)]
    pure ((maskf modMask .|. m, k), f def n)
