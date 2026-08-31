-- | The configuration a user writes. Everything nad reads at runtime that is
-- not the state of the screen lives in here.
module Nad.Types.Config
  ( Config (..)
  , FloatRule (..)
  , BarConfig (..)
  , BarPosition (..)
  , defaultConfig
  , defaultBar
  , shouldFloat
  , bindings
  , reserveBar
  ) where

import Data.List (isInfixOf)
import Data.Maybe (mapMaybe)

import Nad.Bar.Segment (BarContent, BarState, defaultRender)
import Nad.Core.Action (Action (..), Direction (..))
import Nad.Core.Layout (LayoutSpec, defaultLayouts)
import Nad.Types.Geometry (Rect (..))
import Nad.Types.Key (KeyCombo, parseCombo)
import Nad.Types.Window (ScreenInfo (..), WindowInfo (..))

-- | A window matching a rule is left exactly where its app put it.
data FloatRule
  = -- | Application name, matched exactly.
    FloatApp String
  | -- | Window title, matched as a substring.
    FloatTitle String
  deriving (Eq, Show)

data BarPosition = Top | Bottom
  deriving (Eq, Show)

data BarConfig = BarConfig
  { barEnabled :: Bool
  , barPosition :: BarPosition
  , barHeight :: Double
  , barFont :: String
  -- ^ Font name; falls back to the system monospaced font if unavailable.
  , barFontSize :: Double
  , barBackground :: String
  -- ^ @\"#RRGGBB\"@
  , barForeground :: String
  , barRender :: BarState -> BarContent
  -- ^ The whole point of the bar being configurable: a function, not a format
  -- string, so anything expressible in Haskell can go in it.
  }

defaultBar :: BarConfig
defaultBar =
  BarConfig
    { barEnabled = True
    , barPosition = Top
    , barHeight = 26
    , barFont = "SF Mono"
    , barFontSize = 13
    , barBackground = "#1b1b1b"
    , barForeground = "#dddddd"
    , barRender = defaultRender
    }

-- | Take the bar's strip out of a screen's usable area, so layouts never place
-- a window underneath it.
reserveBar :: BarConfig -> ScreenInfo -> ScreenInfo
reserveBar cfg screen
  | not (barEnabled cfg) = screen
  | otherwise = screen {screenUsable = shrink (screenUsable screen)}
  where
    h = barHeight cfg
    shrink r = case barPosition cfg of
      Top -> r {rectY = rectY r + h, rectH = max 0 (rectH r - h)}
      Bottom -> r {rectH = max 0 (rectH r - h)}

data Config = Config
  { cfgKeys :: [(String, Action)]
  -- ^ Bindings as @(\"cmd-alt-j\", action)@. Unparseable names are reported at
  -- start-up rather than silently ignored — see 'bindings'.
  , cfgLayouts :: [LayoutSpec]
  , cfgFloats :: [FloatRule]
  , cfgWorkspaces :: Int
  , cfgBar :: BarConfig
  }

defaultConfig :: Config
defaultConfig =
  Config
    { cfgKeys = defaultKeys
    , cfgLayouts = defaultLayouts
    , cfgFloats = defaultFloats
    , cfgWorkspaces = 9
    , cfgBar = defaultBar
    }

-- | cmd-alt is the modifier: cmd alone collides with nearly every app, and it
-- is one macOS does not already claim for Spaces.
defaultKeys :: [(String, Action)]
defaultKeys =
  [ ("cmd-alt-j", Focus Next)
  , ("cmd-alt-k", Focus Prev)
  , ("cmd-alt-shift-j", Swap Next)
  , ("cmd-alt-shift-k", Swap Prev)
  , ("cmd-alt-return", SwapMaster)
  , ("cmd-alt-h", ResizeMaster (-0.05))
  , ("cmd-alt-l", ResizeMaster 0.05)
  , ("cmd-alt-comma", IncMaster 1)
  , ("cmd-alt-period", IncMaster (-1))
  , ("cmd-alt-space", CycleLayout)
  , ("cmd-alt-r", Retile)
  , ("cmd-alt-shift-left", MoveToScreen Prev)
  , ("cmd-alt-shift-right", MoveToScreen Next)
  , ("cmd-alt-q", Quit)
  ]
    <> [("cmd-alt-" <> show n, View n) | n <- [1 .. 9 :: Int]]
    <> [("cmd-alt-shift-" <> show n, MoveToWorkspace n) | n <- [1 .. 9 :: Int]]

-- | Apps whose windows are dialogs pretending to be windows, or which macOS
-- refuses to resize anyway.
defaultFloats :: [FloatRule]
defaultFloats =
  [ FloatApp "System Settings"
  , FloatApp "Calculator"
  , FloatApp "Finder" -- copy dialogs
  , FloatTitle "Preferences"
  ]

shouldFloat :: [FloatRule] -> WindowInfo -> Bool
shouldFloat rules w = any match rules
  where
    match (FloatApp name) = winApp w == name
    match (FloatTitle needle) = needle `isInfixOf` winTitle w

-- | The bindings that parsed. Anything that did not is returned separately so
-- the caller can complain about it instead of leaving the user with a key that
-- silently does nothing.
bindings :: Config -> ([(KeyCombo, Action)], [String])
bindings cfg = (mapMaybe parseBinding (cfgKeys cfg), bad)
  where
    parseBinding (name, action) = fmap (\combo -> (combo, action)) (parseCombo name)
    bad = [name | (name, _) <- cfgKeys cfg, parseCombo name == Nothing]
