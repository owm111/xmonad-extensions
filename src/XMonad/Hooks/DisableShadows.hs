{-|
The problem: compositors can add shadows to windows. This looks great when
there are gaps between windows, but the shadows will still linger and overlap
other windows when they are tiled togther tightly.

The solution: tell the compositor to ignore windows with the @_DISABLE_SHADOWS@
property is set to @1@, and have XMonad set this property on windows tiled
without gaps.

== Example Usage

For picom, add this to @~\/.config\/picom.conf@:

> shadow-exclude = [
>     "_DISABLE_SHADOWS:8c = 1"
> ];

In @xmonad.hs@:

> import Data.List
> import XMonad.Hooks.DisableShadows
> import XMonad.Layout.Spacing
> 
> myShadowsConf = def { gapsTest = \l -> "Spacing " `isPrefixOf` description l }
> 
> border i = Border i i i i
> 
> myLayoutHook = tall ||| spacing tall ||| Full
>     where spacing = spacingRaw False (border 10) True (border 10) True
>           tall = Tall 1 (3/100) (1/2)
> 
> myConf = def
>     { layoutHook = myLayoutHook
>     , logHook = logHook def >> disableShadows myShadowsConf
>     }
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module XMonad.Hooks.DisableShadows
    ( disableShadowsHook
    , ShadowsConfig (..)
    , Data.Default.def
    ) where

-- Much of the X11 code here is based off of xprop's.

import Control.Monad
import Data.Default
import Data.List
import Data.Map.Strict (Map, member)
import Foreign.C
import Foreign.Marshal hiding (void)
import Graphics.X11.Xlib.Atom
import Graphics.X11.Xlib.Extras
import XMonad
import XMonad.StackSet (StackSet (..), Screen (..), Workspace (..), Stack (..))

-- * Some things that (might) change

-- | Some options for 'disableShadowsHook' that might need to be changed.
data ShadowsConfig = ShadowsConfig
    { propName :: String
    -- ^ The name of the property to set.
    , propType :: Atom
    -- ^ The type of the property to set.
    , propSize :: CInt
    -- ^ The size of the property to set.
    --
    -- This does not change the size/type of the pointer created by 'alloca',
    -- this must be done manually!
    , gapsTest :: forall l a. LayoutClass l a => l a -> Bool
    -- ^ Predicate that checks if a layout has gaps between the windows.
    }

-- | By default, the property set is @_DISABLE_SHADOWS:8c@, and checks for
-- gapped layouts by seeing if @" Spacing "@ is a substring of the layout
-- description.
instance Default ShadowsConfig where
    def = ShadowsConfig
        { propName = "_DISABLE_SHADOWS"
        , propType = cARDINAL
        , propSize = 8
        , gapsTest = \l -> " Spacing " `isInfixOf` description l
        }

-- * Utilities

windowList :: WindowSet -> [Window]
windowList = maybeStackToList . stack . workspace . current

isGapsLayout :: ShadowsConfig -> WindowSet -> Bool
isGapsLayout ShadowsConfig {..} = gapsTest . layout . workspace . current

-- S.integrate' involves reverse-ing, which seems like it could be slow
maybeStackToList :: Maybe (Stack a) -> [a]
maybeStackToList Nothing = []
maybeStackToList (Just (Stack f u d)) = (f : u) ++ d

-- Return the value for the property, given the value for tiled windows and the
-- map of floating windows.
propVal :: Ord a => Bool -> Map a b -> a -> Bool
propVal tiled m x
  | member x m = False
  | otherwise = tiled

-- * Setting properties

-- Set the value of the property for the given window
setProperty :: ShadowsConfig -> Window -> Bool -> X ()
setProperty cfg win val = do
    dpy <- asks display
    io (setProperty' cfg dpy win val)

-- Internal IO calls for 'setProperty'
setProperty' :: ShadowsConfig -> Display -> Window -> Bool -> IO ()
setProperty' ShadowsConfig {..} dpy win val = with (fromBool val) $ \ptr -> do
    atom <- internAtom dpy propName False
    void $ xChangeProperty dpy win atom propType propSize propModeReplace ptr 1

-- * Hook

-- | Hook that sets the @_DISABLE_SHADOWS@ property for each window on the
-- current workspace.
disableShadowsHook :: ShadowsConfig -> X ()
disableShadowsHook cfg = do
    winset <- gets windowset
    let go = propVal (not $ isGapsLayout cfg winset) (floating winset)
    sequence_ [setProperty cfg w (go w) | w <- windowList winset]
