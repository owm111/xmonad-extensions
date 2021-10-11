{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | A layout similar to "XMonad.Layout.ToggleLayouts", except that it allows
-- toggling between three layouts.
--
-- There is one "main" layout (third argument to 'toggle3Layouts') and two other
-- layouts that can be toggled between (using messages from 'ToggleLayout3').
-- For example, if 'ToggleFirstLayout' is sent, the first layout is selected; if
-- 'ToggleSecondLayout' is then sent, then the second layout is selected;  and
-- if 'ToggleSecondLayout' is sent again, then the third layout is selected.
module XMonad.Layout.Toggle3Layouts
    ( toggle3Layouts
    , ToggleLayout3 (..)
    , Toggle3Layouts
    ) where

import Data.Maybe
import XMonad
import XMonad.StackSet (Workspace (..))

data ThreeWay = X | Y | Z deriving (Eq, Read, Show)

data ToggleLayout3 = ToggleFirstLayout | ToggleSecondLayout
    deriving (Eq, Read, Show, Typeable)

instance Message ToggleLayout3

data Toggle3Layouts x y z a = Toggle3Layouts ThreeWay (x a) (y a) (z a)
    deriving (Eq, Read, Show)

toggle3Layouts = Toggle3Layouts Z

instance (LayoutClass x a, LayoutClass y a, LayoutClass z a) =>
    LayoutClass (Toggle3Layouts x y z) a where

    runLayout (Workspace i (Toggle3Layouts X x y z) ms) r = do
        (ws, mx) <- runLayout (Workspace i x ms) r
        pure (ws, fmap (\x' -> Toggle3Layouts X x' y z) mx)

    runLayout (Workspace i (Toggle3Layouts Y x y z) ms) r = do
        (ws, my) <- runLayout (Workspace i y ms) r
        pure (ws, fmap (\y' -> Toggle3Layouts Y x y' z) my)

    runLayout (Workspace i (Toggle3Layouts Z x y z) ms) r = do
        (ws, mz) <- runLayout (Workspace i z ms) r
        pure (ws, fmap (\z' -> Toggle3Layouts Z x y z') mz)

    description (Toggle3Layouts X l _ _) = description l
    description (Toggle3Layouts Y _ l _) = description l
    description (Toggle3Layouts Z _ _ l) = description l

    handleMessage (Toggle3Layouts w x y z) m
      | Just ReleaseResources <- fromMessage m = do
          mx <- handleMessage x m
          my <- handleMessage y m
          mz <- handleMessage z m
          pure $ case (mx, my, mz) of
                   (Nothing, Nothing, Nothing) -> Nothing
                   _ -> let x' = fromMaybe x mx 
                            y' = fromMaybe y my
                            z' = fromMaybe z mz
                         in Just (Toggle3Layouts w x' y' z')
      | Just ToggleFirstLayout <- fromMessage m =
          case w of
            X -> do
                mx <- handleMessage x (SomeMessage Hide)
                let x' = fromMaybe x mx
                pure (Just $ Toggle3Layouts Z x' y z)
            Y -> do
                my <- handleMessage y (SomeMessage Hide)
                let y' = fromMaybe y my
                pure (Just $ Toggle3Layouts X x y' z)
            Z -> do
                mz <- handleMessage z (SomeMessage Hide)
                let z' = fromMaybe z mz
                pure (Just $ Toggle3Layouts X x y z')
      | Just ToggleSecondLayout <- fromMessage m =
          case w of
            Y -> do
                my <- handleMessage y (SomeMessage Hide)
                let y' = fromMaybe y my
                pure (Just $ Toggle3Layouts Z x y' z)
            X -> do
                mx <- handleMessage x (SomeMessage Hide)
                let x' = fromMaybe x mx
                pure (Just $ Toggle3Layouts Y x' y z)
            Z -> do
                mz <- handleMessage z (SomeMessage Hide)
                let z' = fromMaybe z mz
                pure (Just $ Toggle3Layouts Y x y z')
      | otherwise =
          case w of
            X -> do
                mx <- handleMessage x m
                pure $ fmap (\x' -> Toggle3Layouts w x' y z) mx
            Y -> do
                my <- handleMessage y m
                pure $ fmap (\y' -> Toggle3Layouts w x y' z) my
            Z -> do
                mz <- handleMessage z m
                pure $ fmap (\z' -> Toggle3Layouts w x y z') mz
