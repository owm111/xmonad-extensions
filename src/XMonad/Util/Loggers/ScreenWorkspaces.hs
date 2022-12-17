{-|
On a setup with more than two or three monitors, it can be difficult to
quickly determine which screen a certian workspace is visible on based on
a status bar. This module aims to fix that by showing the screens with
labels and in known positions with their workspace before all other,
hidden workspaces.
-}

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

module XMonad.Util.Loggers.ScreenWorkspaces where

import Control.Monad.Trans.Maybe
import Data.List
import Data.Maybe
import XMonad
import XMonad.StackSet qualified as W
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare

{-|
'screenWorkspacesPP' is a "drop in" solution, and reuses the
given pretty printer as much as possible.

__XXX:__ This does nothing special to handle urgent, visible
workspaces. They
are displayed twice.
-}
screenWorkspacesPP ::
    {-| The labels or names of the screens to display, in "physical"
    order. See "XMonad.Actions.PhysicalScreens". -}
    [label] ->
    {-| Function to merge a label and workspace. If the labels are just
    strings, then 'insertBetween' can be used to insert a string between
    the two.  -}
    (label -> WorkspaceId -> WorkspaceId) ->
    {-| The base pretty printer to modify. -}
    PP ->
    PP
screenWorkspacesPP names merge pp@PP {..} = pp
    { ppCurrent = const ""
    , ppVisible = const ""
    , ppExtras = screenWorkspacesLogger names current visible ppWsSep : ppExtras
    , ppOrder = ppOrder . \(w : l : t : f : rest) -> (f ++ w) : l : t : rest
    } where
        current s w = ppCurrent (merge s w)
        visible s w = ppVisible (merge s w)

{-| Insert a string between two others. -}
insertBetween y x z = x ++ y ++ z

{- |
'screenWorkspacesLogger' is just a 'Logger' that provides the core
functionality of logging screens with workspaces.
-}
screenWorkspacesLogger ::
    {-| The labels or names of the screens to display, in "physical"
    order. See "XMonad.Actions.PhysicalScreens". -}
    [label] ->
    {-| Function to format the currently focused screen and
    workspace. Analogous to 'ppCurrent'. -}
    (label -> WorkspaceId -> String) ->
    {-| Function to format any visible screens and workspaces. Analogous
    to 'ppVisible'. -}
    (label -> WorkspaceId -> String) ->
    {-| Seperator string; inserted between screen-workspace
    pairs. Analogous to 'ppWsSep'. -}
    String ->
    Logger
screenWorkspacesLogger names current visible sep = do
    let workspaceOn (ps, name) = runMaybeT $ do
            sid <- MaybeT (getScreen def ps)
            wksp <- MaybeT (logCurrentOnScreen sid)
            c <- gets (W.currentTag . windowset)
            pure ((if wksp == c then current else visible) name wksp)
    maybes <- traverse workspaceOn (zip [0 ..] names)
    let string = intercalate sep (catMaybes maybes)
    pure (Just string)
