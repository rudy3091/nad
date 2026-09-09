-- A nad configuration using every setting once, as a starting point to copy
-- into ~/.nad/nad.hs and cut down.
--
-- It is also the check on the reference in SKILL.md: if a field name or a
-- signature there has drifted from the library, this stops compiling. From a
-- nad checkout, no install needed:
--
--   cabal exec -- ghc -fno-code .claude/skills/nad-config/example.hs
module Main (main) where

import Nad

main :: IO ()
main = nadWith config

config :: Config
config =
  defaultConfig
    { cfgWorkspaces = 5
    , -- Extend the defaults rather than replacing them: defaultKeys is not
      -- exported, so defaultConfig is how you read it back.
      cfgKeys =
        cfgKeys defaultConfig
          <> [ ("cmd-alt-t", Retile)
             , -- The keyboard's display, not the window's: cmd-alt-shift-right
               -- is the one that takes the window along.
               ("cmd-alt-w", FocusScreen Prev)
             , ("cmd-alt-e", FocusScreen Next)
             ]
    , -- Tall and Full only, so cmd-alt-space toggles between two layouts.
      cfgLayouts = [LayoutSpec Tall 0.6 1 10, LayoutSpec Full 0.6 1 0]
    , cfgFloats = RuleApp "Activity Monitor" : cfgFloats defaultConfig
    , cfgAssign = [(RuleApp "kitty", 1), (RuleApp "Safari", 2)]
    , cfgPin = [(RuleApp "kitty", 0)]
    , cfgBar =
        defaultBar
          { barPosition = Bottom
          , barHeight = 24
          , barFont = "SF Mono"
          , barFontSize = 12
          , barBackground = "#1b1b1b"
          , barForeground = "#dddddd"
          , barRender = render
          }
    , cfgBorder = defaultBorder {borderWidth = 4, borderColor = "#88aaff"}
    }

-- | Workspaces on the left, the focused window in the middle, layout and clock
-- on the right. A pure function: BarState is everything it gets to know.
render :: BarState -> BarContent
render st =
  BarContent
    { barLeft = map chip (bsWorkspaces st)
    , barCenter = [seg (bsFocused st)]
    , barRight = [seg (bsLayout st), seg "  ", colored "#88aaff" (bsClock st)]
    }
  where
    -- Three states worth telling apart with two displays: showing here,
    -- showing on the other one, not showing at all.
    chip (wid, showing, count)
      | showing = colored "#88aaff" (label wid)
      | wid `elem` bsOtherScreens st = colored "#5a7fbf" (label wid)
      | count > 0 = seg (label wid)
      | otherwise = colored "#555555" (label wid)
    label wid = " " <> show wid <> " "
