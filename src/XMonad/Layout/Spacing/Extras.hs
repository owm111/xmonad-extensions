{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Some utilities for the Spacing layout modifier.
module XMonad.Layout.Spacing.Extras
    ( border 
    , SpacingDesc
    , spacingDescRaw
    , module XMonad.Layout.Spacing
    ) where

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing

-- | Create a border with equal sides.
border i = Border i i i i

-- | A wrapper for 'Spacing' that changes its description to @NoSpacing@ if
-- 'windowSpacingEnabled' is false.
newtype SpacingDesc a = SpacingDesc { getSpacing :: Spacing a }
    deriving (Read, Show)

instance Eq a => LayoutModifier SpacingDesc a where
    modifyLayout (SpacingDesc spacing) = modifyLayout spacing
    pureMess (SpacingDesc spacing) m = SpacingDesc <$> pureMess spacing m
    pureModifier (SpacingDesc spacing) r ms rs =
        let (rs', ml) = pureModifier spacing r ms rs
         in (rs', fmap SpacingDesc ml)

    modifierDescription (SpacingDesc spacing)
      | windowBorderEnabled spacing = "Spacing"
      | otherwise = "NoSpacing"

-- | Modifies a layout with 'SpacingDesc'. Takes same parameters as 'spacingRaw'
spacingDescRaw v w x y z = ModifiedLayout (SpacingDesc $ Spacing v w x y z)
