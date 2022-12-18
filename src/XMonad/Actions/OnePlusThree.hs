{-|
This module provides actions for quickly switching workspaces on all
screens, keeping one "fixed" while the others change to a set of three
workspaces. It is intended for use on a 1+3 setup, with one monitor
above a row of three.

Similar to "XMonad.Actions.Workscreen", but this module allows keeping
a workscreen fixed to a screen and does not yet allow customization of
which workspaces are apart of each set.

Possible TODO:

- Generalize to any monitor setup
- Different ways to select which workspaces are part of each set
- Deal with dynamic workspaces
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module XMonad.Actions.OnePlusThree
    ( viewWorkspaceSet
    , workspaceSetKeys
    ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Map.Strict qualified as M
import XMonad
import XMonad.Actions.OnScreen
import XMonad.Actions.PhysicalScreens
import XMonad.StackSet qualified as W
import XMonad.Util.Keymap

{-|
View the given workspace set. Workspace set /n/ is defined as workspace 0
on screen 0, workspace 3/n/+1 on screen 1, workspace 3/n/+2 on screen 2,
and workspace 3/n/+3 on screen 3.
-}
viewWorkspaceSet ::
    {-| Value of /n/. -}
    Int ->
    X ()
viewWorkspaceSet = fmap (const ()) . runMaybeT . viewWorkspaceSet'

viewWorkspaceSet' n = do
    screens <- traverse (MaybeT . getScreen def) [0 .. 3]
    wksp0 : wksps <- asks (workspaces . config)
    otherWorkspaces@[_, middle, _] <- pure . take 3 . drop (3 * n) $ wksps
    mapM_ goPair (zip screens (wksp0 : otherWorkspaces))
    lift (windows (W.view middle))

goPair (screen, wksp) = lift (windows (greedyViewOnScreen screen wksp))

{-|
Generate a 'Keymap' of 'viewWorkspaceSet' for all workspaces with
the same mask and a given set of symbols.
-}
workspaceSetKeys ::
    {-| The mask to use for all the binds of this map. -}
    KeyMaskFn ->
    {-| List of key symbols to use for mappings. To use the number keys,
    use @[xK_1 ..]@. -}
    [KeySym] ->
    Keymap
workspaceSetKeys maskf keys XConfig {modMask, workspaces} = M.fromList $ do
    (i, key) <- zip [0 .. (length workspaces - 1) `div` 3] keys
    pure ((maskf modMask, key), viewWorkspaceSet i)
