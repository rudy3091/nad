-- | Checks for the pure half of nad. Anything that needs macOS is verified
-- through the CLI instead (@nad doctor@, @nad query ...@).
module Main (main) where

import Control.Monad (unless)
import System.Exit (exitFailure)

import Nad.Bar.Segment
import Nad.Cli (Command (..), parseCommand)
import Nad.Core.Action
import Nad.Core.Layout
import Nad.Core.Stack (Stack (..))
import qualified Nad.Core.Stack as Stack
import Nad.Core.State
import Nad.Types.Geometry
import Nad.Types.Config (BarConfig (..), BarPosition (..), defaultBar, reserveBar)
import Nad.Types.Key
import Nad.Types.Window (ScreenInfo (..), screenFor)

main :: IO ()
main = do
  results <- mapM runCheck checks
  unless (and results) exitFailure
  putStrLn (show (length checks) <> " checks passed")

runCheck :: (String, Bool) -> IO Bool
runCheck (label, ok) = do
  putStrLn ((if ok then "ok   " else "FAIL ") <> label)
  pure ok

screen :: Rect
screen = Rect 0 0 1440 900

-- A gapless Tall keeps the arithmetic exact, so the checks below can compare
-- rectangles rather than allow for slack.
tallSpec :: LayoutSpec
tallSpec = LayoutSpec Tall 0.5 1 0

tileTall :: Int -> [Rect]
tileTall n = map snd (arrange tallSpec screen [1 .. n :: Int])

stackSpec :: LayoutSpec
stackSpec = LayoutSpec Stacking 0.5 1 0

-- | Three windows the layout has placed, one of which the user then dragged —
-- @f@ turns its placed rectangle into the one it really has now — and the
-- placements that drag implies.
adopts :: LayoutSpec -> Int -> (Rect -> Rect) -> [(Int, Placement)]
adopts = adoptsWith (const noPlacement)

adoptsWith :: Sizing Int -> LayoutSpec -> Int -> (Rect -> Rect) -> [(Int, Placement)]
adoptsWith sizing spec target f = draggedPlacements spec screen actual placed
  where
    placed = arrangeWith spec screen sizing [1 .. 3 :: Int]
    actual w = (if w == target then f else id) <$> lookup w placed

-- | Feeding an adopted placement back into the layout must land on the very
-- rectangle it came from: that is what stops a drag from creeping every poll.
replaces :: LayoutSpec -> Int -> (Rect -> Rect) -> Bool
replaces spec target f = case adopts spec target f of
  [(w, p)] -> lookup w (arrangeWith spec screen (\x -> if x == w then p else noPlacement) [1 .. 3 :: Int]) == Just (f dragged)
    where
      dragged = maybe screen id (lookup w (arrange spec screen [1 .. 3 :: Int]))
  _ -> False

-- | A size override and nothing else, which is all the resize keys set.
sized :: (Double, Double) -> Sizing w
sized s = const (Placement Nothing (Just s))

-- | A drag of the left-hand edge: narrower, same corner.
narrower :: Rect -> Rect
narrower r = r {rectW = rectW r - 720}

-- | A drag of the top-right corner towards the bottom left: the size and the
-- top edge both move.
cornerwards :: Rect -> Rect
cornerwards r = Rect (rectX r) (rectY r + 450) (rectW r - 720) (rectH r - 450)

pairs :: [a] -> [(a, a)]
pairs xs = [(a, b) | (i, a) <- indexed, (j, b) <- indexed, i < (j :: Int)]
  where
    indexed = zip [0 ..] xs

checks :: [(String, Bool)]
checks =
  [ -- geometry
    ("y flip round-trips", cocoaToAxY 1000 200 (axToCocoaY 1000 200 130) == 130)
  , ("y flip maps a top-left window to a bottom-left origin", axToCocoaY 1000 200 0 == 800)
  , ("contains accepts a nested rect", screen `contains` Rect 10 10 100 100)
  , ("contains rejects an overhanging rect", not (screen `contains` Rect 10 10 2000 100))
  , ("contains accepts an exact fit", screen `contains` screen)
  , ("intersects finds an overlap", Rect 0 0 100 100 `intersects` Rect 50 50 100 100)
  , ("tiled neighbours do not intersect", not (Rect 0 0 100 100 `intersects` Rect 100 0 100 100))
  , ("inset shrinks on every side", inset 10 screen == Rect 10 10 1420 880)
  , ("inset never goes negative", rectW (inset 5000 screen) == 0)
  , ("a point on the far edge is outside", not (screen `containsPoint` Point 1440 0))
  , -- layout
    ("empty input places nothing", null (tileTall 0))
  , ("one window gets the whole area", tileTall 1 == [screen])
  , ("every window is placed", length (tileTall 5) == 5)
  , ("tiles stay inside the area", all (screen `contains`) (tileTall 5))
  , ("tiles do not overlap", not (any (uncurry intersects) (pairs (tileTall 5))))
  , ("master takes its share of the width", map rectW (tileTall 2) == [720, 720])
  , ("stack windows split the height", map rectH (tileTall 3) == [900, 450, 450])
  , ("an extreme master ratio still leaves both columns", all ((> 0) . rectW) (map snd (arrange tallSpec {specMasterRatio = 99} screen [1 .. 3 :: Int])))
  , ("Full gives everyone the whole area", map snd (arrange (LayoutSpec Full 0.5 1 0) screen [1 .. 3 :: Int]) == replicate 3 screen)
  , ("Stacking offsets each window", length (nubRects (map snd (arrange (LayoutSpec Stacking 0.5 1 0) screen [1 .. 3 :: Int]))) == 3)
  , ("a window with no size of its own fills what is left of the area", stackedWidths (const noPlacement) == [1440, 1412, 1384])
  , ("a size override is exactly its fraction of the area", stackedWidths (sized (0.5, 1)) == replicate 3 720)
  , ("height sizing is independent of width", stackedWidths (sized (1, 0.5)) == stackedWidths (sized (1, 1)))
  , ("sizing only touches the window it names", stackedWidths (\w -> if w == 2 then Placement Nothing (Just (0.5, 1)) else noPlacement) == [1440, 720, 1384])
  , ("Tall ignores per-window sizing", map snd (arrangeWith tallSpec screen (sized (0.2, 0.2)) [1 .. 3 :: Int]) == tileTall 3)
  , ("layout cycling wraps back around", iterate nextLayout defaultLayouts !! length defaultLayouts == defaultLayouts)
  , -- mouse resize in Stacking
    ("a size drag is read as a fraction of the area", map (placeSize . snd) (adopts stackSpec 1 narrower) == [Just (0.5, 1)])
  , ("a corner drag is read as an origin too", map (placeOrigin . snd) (adopts stackSpec 1 cornerwards) == [Just (0, 0.5)])
  , ("re-placing an adopted size drag lands on the same rectangle", replaces stackSpec 2 narrower)
  , ("re-placing an adopted corner drag lands on the same rectangle", replaces stackSpec 2 cornerwards)
  , ("the gap is undone on the way in and back out", replaces stackSpec {specGap = 8} 2 cornerwards)
  , ("a drag that only moved a window is left to the layout", null (adopts stackSpec 2 (\r -> r {rectX = rectX r + 300})))
  , ("a window an app nudged by a few pixels is not a drag", null (adopts stackSpec 2 (\r -> r {rectW = rectW r - 8, rectH = rectH r + 8})))
  , ("only the window the user dragged is adopted", map fst (adopts stackSpec 2 narrower) == [2])
  , ("the tiled layouts have nowhere to put a drag", null (adopts tallSpec 2 narrower) && null (adopts (LayoutSpec Full 0.5 1 0) 2 narrower))
  , -- screen assignment
    ("a window is assigned to the screen holding its centre", fmap screenIndex (screenFor twoScreens (Rect 1500 100 400 300)) == Just 1)
  , ("a window off every screen is unassigned", screenFor twoScreens (Rect 9000 9000 10 10) == Nothing)
  , -- stack
    ("focus follows the order", stackFocus (Stack.focusNext three) == Just 2)
  , ("focus wraps at the end", stackFocus (Stack.focusNext (Stack.setFocus 3 three)) == Just 1)
  , ("focus wraps backwards", stackFocus (Stack.focusPrev three) == Just 3)
  , ("swapping reorders the windows", stackItems (Stack.swapNext three) == [2, 1, 3])
  , ("the focus travels with a swapped window", stackFocus (Stack.swapNext three) == Just 1)
  , ("swap-master promotes the focused window", stackItems (Stack.swapMaster (Stack.setFocus 3 three)) == [3, 2, 1])
  , ("swap-master on master swaps with the next window", stackItems (Stack.swapMaster three) == [2, 1, 3])
  , ("sync keeps known windows in place", stackItems (Stack.sync [3, 1, 2] three) == [1, 2, 3])
  , ("sync drops closed windows", stackItems (Stack.sync [1, 3] three) == [1, 3])
  , ("sync puts new windows first", stackItems (Stack.sync [1, 2, 3, 9] three) == [9, 1, 2, 3])
  , ("a closed focus falls back to master", stackFocus (Stack.sync [2, 3] three) == Just 2)
  , ("removing is idempotent", Stack.remove 1 (Stack.remove 1 three) == Stack.remove 1 three)
  , -- state
    ("actions apply to the current workspace", stackItems (currentStack (apply SwapMaster loaded)) == [2, 1, 3])
  , ("viewing an unknown workspace is ignored", stCurrent (apply (View 99) loaded) == 1)
  , ("moving a window to another workspace removes it here", stackItems (currentStack (apply (MoveToWorkspace 2) loaded)) == [2, 3])
  , ("...and adds it there", workspaceOf 1 (apply (MoveToWorkspace 2) loaded) == Just 2)
  , ("sync leaves other workspaces alone", workspaceOf 1 (syncWindows [1, 2, 3] (apply (MoveToWorkspace 2) loaded)) == Just 2)
  , ("resizing a window records a size for it", placeSize (placeOf (apply (ResizeWindow (-0.1) 0) loaded) 1) == Just (0.9, 1))
  , ("window resizing is clamped", placeSize (placeOf (times 30 (apply (ResizeWindow (-0.05) 0)) loaded) 1) == Just (0.2, 1))
  , ("sizes accumulate on the focused window only", placeSize (placeOf (apply (ResizeWindow 0 (-0.1)) loaded) 2) == Nothing)
  , ("a closed window's size is forgotten", placeSize (placeOf (syncWindows [2, 3] (apply (ResizeWindow (-0.1) 0) loaded)) 1) == Nothing)
  , ("a keyboard resize leaves the origin to the layout", placeOrigin (placeOf (apply (ResizeWindow (-0.1) 0) loaded) 1) == Nothing)
  , ("resizing master is clamped", specMasterRatio (currentLayout (times 20 (apply (ResizeMaster 0.05)) loaded)) <= 0.9)
  , ("master count never drops below one", specMasterCount (currentLayout (times 5 (apply (IncMaster (-1))) loaded)) == 1)
  , ("quit clears the running flag", not (stRunning (apply Quit loaded)))
  , -- keys and actions
    ("a combo round-trips", (parseCombo "cmd-alt-j" >>= parseCombo . showCombo) == parseCombo "cmd-alt-j")
  , ("modifier order does not matter", parseCombo "alt-cmd-j" == parseCombo "cmd-alt-j")
  , ("an unknown key name is rejected", parseCombo "cmd-nope" == Nothing)
  , ("a system shortcut on the same combo is reported", conflicting [space] [(65, space), (52, dee)] == [65])
  , ("unrelated system shortcuts are left alone", conflicting [space] [(52, dee)] == [])
  , ("no bindings means nothing to take over", conflicting [] [(65, space)] == [])
  , ("modifier bits round-trip", unpackModifiers (packModifiers [Cmd, Shift]) == [Cmd, Shift])
  , ("every named action parses back", all (\(n, a) -> parseAction [n] == Just a) actionNames)
  , ("every named action prints back", all (\(n, a) -> showAction a == n) actionNames)
  , ("an action with an argument round-trips", parseAction (words (showAction (View 3))) == Just (View 3))
  , ("garbage is not an action", parseAction ["nope"] == Nothing)
  , -- bar
    ("segments encode one record per segment", length (splitOn '\x1e' (encodeSegments [seg "a", seg "b"])) == 2)
  , ("a segment encodes three fields", length (splitOn '\x1f' (encodeSegments [colored "#ff0000" "x"])) == 3)
  , ("separators in a window title cannot corrupt the encoding", length (splitOn '\x1e' (encodeSegments [seg "a\x1eb"])) == 1)
  , ("an empty bar encodes to nothing", encodeSegments [] == "")
  , ("the default render marks the current workspace", barLeft (defaultRender sampleBar) /= [])
  , ("a top bar takes height off the top", screenUsable (reserveBar defaultBar oneScreen) == Rect 0 26 1440 874)
  , ("a disabled bar takes nothing", screenUsable (reserveBar defaultBar {barEnabled = False} oneScreen) == screen)
  , ("a bottom bar leaves the top alone", rectY (screenUsable (reserveBar defaultBar {barPosition = Bottom} oneScreen)) == 0)
  , -- cli
    ("query windows parses", parseCommand ["query", "windows"] == QueryWindows)
  , ("no arguments runs the daemon", parseCommand [] == Daemon)
  , ("garbage is rejected", parseCommand ["wat"] == Unknown ["wat"])
  ]
  where
    three = Stack.fromList [1, 2, 3 :: Int]
    -- A gapless Stacking layout, so only the cascade offset moves the edges.
    stackedWidths sizing =
      map (rectW . snd) (arrangeWith (LayoutSpec Stacking 0.5 1 0) screen sizing [1 .. 3 :: Int])
    oneScreen = ScreenInfo 0 screen screen
    Just space = parseCombo "cmd-alt-space"
    Just dee = parseCombo "cmd-alt-d"
    sampleBar = BarState [(1, True, 2), (2, False, 0)] "Tall" "editor" 0 "14:32"
    splitOn sep xs = case break (== sep) xs of
      (chunk, []) -> [chunk]
      (chunk, _ : rest) -> chunk : splitOn sep rest
    loaded = (initialState 9 defaultLayouts) {stWorkspaces = [Workspace 1 three, Workspace 2 Stack.empty]}
    times n f = foldr (.) id (replicate n f)
    nubRects = foldr (\r acc -> if r `elem` acc then acc else r : acc) []
    twoScreens =
      [ ScreenInfo 0 screen screen
      , ScreenInfo 1 (Rect 1440 0 1440 900) (Rect 1440 0 1440 900)
      ]
