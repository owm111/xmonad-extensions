{-|
Allows you to cycle between various actions. Each workspace has its own state.

== Example

I want to have "simple" layout (no spacing and theme @simpleTheme@) and
"fancy" layout (spacing and theme @fancyTheme@).

> data SettingsCycle = TSSimple | TSFancy
>     deriving (Bounded, Enum, Eq, Read, Show, Typeable)
> 
> instance SettingsCycleClass SettingsCycle where
>     switchToTheme TSSimple = do
>         sendMessage (SetTheme simpleTheme)
>         setWindowSpacingEnabled False
>         setScreenSpacingEnabled False
> 
>     switchToTheme TSFancy = do
>         sendMessage (SetTheme fancyTheme)
>         setWindowSpacingEnabled True
>         setScreenSpacingEnabled True

In keybindings...

> , ((modm, xK_g), toggleTheme (Proxy :: Proxy SettingsCycle))

This allows me to press M-g to toggle spacing and a theme on and off on the
current workspace.
-}

module XMonad.Actions.CycleSettings
    ( SettingsCycleClass (..)
    , cycleSettings
    , Data.Proxy.Proxy (..)
    , SettingsCycles (getMap)
    ) where

import Data.Proxy
import XMonad
import qualified Data.Map.Strict as M
import qualified XMonad.StackSet as S
import qualified XMonad.Util.ExtensibleState as XS

{-|

Note that:

    - 'minBound' is the default setting
    - 'succ' is used to go the next setting
    - @('==' 'maxBound')@ is used to determine if the last setting has been
        reached
-}
class (Bounded a, Enum a, Eq a, Read a, Show a, Typeable a) => SettingsCycleClass a where
    -- | Switch to the given theme settings.
    switchToSettings :: a -> X ()

defOnSwitch :: SettingsCycleClass a => a
defOnSwitch = succ' minBound

switchToSettings' :: SettingsCycleClass a => a -> X a
switchToSettings' ts = ts <$ switchToSettings ts

{- |
The type used internally to store mappings of workspaces to their respective,
current settings.
-}
newtype SettingsCycles ts = SettingsCycles { getMap :: M.Map String ts }
    deriving (Read, Show, Typeable)

instance SettingsCycleClass ts => ExtensionClass (SettingsCycles ts) where
    initialValue = SettingsCycles mempty
    extensionType = PersistentExtension

cycleWorkspace :: SettingsCycleClass ts => String -> SettingsCycles ts -> X (SettingsCycles ts)
cycleWorkspace k m = SettingsCycles <$>
    M.alterF (fmap Just . switchToSettings' . maybe defOnSwitch succ') k (getMap m)

currentTag :: XState -> String
currentTag = S.tag . S.workspace . S.current . windowset

{-|
An action (that can be bound to a key) that switches to the next theme settings.
-}
cycleSettings :: SettingsCycleClass ts => Proxy ts -> X ()
cycleSettings proxy =
    (,) <$> gets currentTag <*> XS.gets (`asProxyTypeOf` proxy') >>= uncurry cycleWorkspace >>= XS.put
        where proxy' = fmap (SettingsCycles . M.singleton "") proxy

succ' :: (Bounded e, Enum e, Eq e) => e -> e
succ' x
  | x == maxBound = minBound
  | otherwise = succ x
