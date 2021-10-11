
{-# LANGUAGE FlexibleContexts #-}

-- | Some utilities for DyanmicLog.
module XMonad.Hooks.DynamicLog.Extras
    ( xmobarWithPP
    , module XMonad.Hooks.DynamicLog
    ) where

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

-- | A version of 'xmobar' that accepts a custom pretty-printer.
xmobarWithPP
    :: LayoutClass l Window
    => PP
    -> XConfig l
    -> IO (XConfig (ModifiedLayout AvoidStruts l))
xmobarWithPP pp xc = statusBar "xmobar" pp strutsKey xc
    where strutsKey XConfig { modMask = m } = (m, xK_b)
