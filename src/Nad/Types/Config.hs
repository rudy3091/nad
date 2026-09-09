-- | The configuration a user writes. Everything nad reads at runtime that is
-- not the state of the screen lives in here.
module Nad.Types.Config
  ( Config (..)
  , Rule (..)
  , BarConfig (..)
  , BarPosition (..)
  , defaultConfig
  , defaultBar
  , matches
  , ruleValue
  , shouldFloat
  , bindings
  , reserveBar
  ) where

import Data.List (isInfixOf)
import Data.Maybe (listToMaybe, mapMaybe)

import Nad.Bar.Segment (BarContent, BarState, defaultRender)
import Nad.Core.Action (Action (..), Direction (..))
import Nad.Core.Layout (LayoutSpec, defaultLayouts)
import Nad.Types.Geometry (Rect (..), rectBottom)
import Nad.Types.Key (KeyCombo, parseCombo)
import Nad.Types.Window (ScreenInfo (..), WindowInfo (..))

-- | How a rule picks the windows it applies to. Floating, workspace assignment
-- and display pinning all match the same way.
data Rule
  = -- | Application name, matched exactly.
    RuleApp String
  | -- | Window title, matched as a substring.
    RuleTitle String
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
      -- A top bar is drawn at the top of the display, over the menu bar, so
      -- only the part of it reaching past the menu bar has to be taken away.
      -- A bar shorter than the menu bar costs nothing at all.
      Top ->
        let top = max (rectY r) (rectY (screenFrame screen) + h)
         in r {rectY = top, rectH = max 0 (rectBottom r - top)}
      Bottom -> r {rectH = max 0 (rectH r - h)}

data Config = Config
  { cfgKeys :: [(String, Action)]
  -- ^ Bindings as @(\"cmd-alt-j\", action)@. Unparseable names are reported at
  -- start-up rather than silently ignored — see 'bindings'.
  , cfgLayouts :: [LayoutSpec]
  , cfgFloats :: [Rule]
  -- ^ Windows left exactly where their app put them.
  , cfgAssign :: [(Rule, Int)]
  -- ^ The workspace a window opens on, as @(rule, workspace)@. Applies the
  -- first time nad sees a window and never again, so moving one by hand sticks.
  , cfgPin :: [(Rule, Int)]
  -- ^ The display a window is kept on, as @(rule, screen index)@. Index 0 is
  -- the display holding the menu bar. A rule naming a display that is not
  -- attached is ignored.
  , cfgWorkspaces :: Int
  , cfgBar :: BarConfig
  }

defaultConfig :: Config
defaultConfig =
  Config
    { cfgKeys = defaultKeys
    , cfgLayouts = defaultLayouts
    , cfgFloats = defaultFloats
    , cfgAssign = []
    , cfgPin = []
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
  -- macOS claims this one for Finder search. nad switches that system shortcut
  -- off while it runs and puts it back on exit, so the xmonad key still works.
  , ("cmd-alt-space", CycleLayout)
  -- Resizing the focused window itself, for the Stacking layout. Vim
  -- directions, one modifier deeper than the master-area keys they echo.
  , ("cmd-alt-ctrl-h", ResizeWindow (-0.05) 0)
  , ("cmd-alt-ctrl-l", ResizeWindow 0.05 0)
  , ("cmd-alt-ctrl-k", ResizeWindow 0 (-0.05))
  , ("cmd-alt-ctrl-j", ResizeWindow 0 0.05)
  , ("cmd-alt-r", Retile)
  , ("cmd-alt-shift-left", MoveToScreen Prev)
  , ("cmd-alt-shift-right", MoveToScreen Next)
  , ("cmd-alt-q", Quit)
  ]
    <> [("cmd-alt-" <> show n, View n) | n <- [1 .. 9 :: Int]]
    <> [("cmd-alt-shift-" <> show n, MoveToWorkspace n) | n <- [1 .. 9 :: Int]]

-- | Apps whose windows are dialogs pretending to be windows, or which macOS
-- refuses to resize anyway.
defaultFloats :: [Rule]
defaultFloats =
  [ RuleApp "System Settings"
  , RuleApp "Calculator"
  , RuleApp "Finder" -- copy dialogs
  , RuleTitle "Preferences"
  ]

matches :: Rule -> WindowInfo -> Bool
matches (RuleApp name) w = winApp w == name
matches (RuleTitle needle) w = needle `isInfixOf` winTitle w

shouldFloat :: [Rule] -> WindowInfo -> Bool
shouldFloat rules w = any (`matches` w) rules

-- | What a table of rules says about a window. The first match wins, so order
-- is priority.
ruleValue :: [(Rule, Int)] -> WindowInfo -> Maybe Int
ruleValue rules w = listToMaybe [n | (rule, n) <- rules, matches rule w]

-- | The bindings that parsed. Anything that did not is returned separately so
-- the caller can complain about it instead of leaving the user with a key that
-- silently does nothing.
bindings :: Config -> ([(KeyCombo, Action)], [String])
bindings cfg = (mapMaybe parseBinding (cfgKeys cfg), bad)
  where
    parseBinding (name, action) = fmap (\combo -> (combo, action)) (parseCombo name)
    bad = [name | (name, _) <- cfgKeys cfg, parseCombo name == Nothing]
