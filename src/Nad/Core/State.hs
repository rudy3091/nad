-- | The window manager's state and how actions change it. Pure: the runtime
-- takes the resulting state and makes the screen match it.
module Nad.Core.State
  ( WMState (..)
  , Workspace (..)
  , initialState
  , stCurrent
  , workspaceOn
  , stackOn
  , visibleWorkspaces
  , syncScreens
  , currentStack
  , currentLayout
  , placeOf
  , resize
  , place
  , syncWindows
  , focusedWindow
  , workspaceOf
  , apply
  ) where

import Data.List (elemIndex, find)
import Data.Maybe (fromMaybe)

import Nad.Core.Action (Action (..), Direction (..))
import Nad.Core.Layout (Layout (..), LayoutSpec (..), Placement (..), nextLayout, noPlacement)
import Nad.Core.Stack (Stack (..))
import qualified Nad.Core.Stack as Stack

data Workspace w = Workspace
  { wsId :: !Int
  , wsStack :: !(Stack w)
  }
  deriving (Eq, Show)

data WMState w = WMState
  { stWorkspaces :: ![Workspace w]
  , -- | Which workspace each attached display is showing, as
    -- @(screen index, workspace id)@, in the order macOS reports the displays.
    -- No two displays ever show the same workspace — that is what makes
    -- \"which display is this window on\" answerable from state alone rather
    -- than from the window's coordinates.
    stVisible :: ![(Int, Int)]
  , -- | The display keyboard actions apply to. Only nad's own actions move it:
    -- there is no way to read macOS's focused window, so a mouse click on
    -- another display goes unnoticed until the next 'FocusScreen'.
    stScreen :: !Int
  -- | Head is the layout in use; 'CycleLayout' rotates the list.
  , stLayouts :: ![LayoutSpec]
  -- | Per-window placement overrides, as fractions of the screen. Only windows
  -- the user has actually resized or dragged appear here, and only
  -- 'Nad.Core.Layout.Stacking' reads them.
  , stPlaces :: ![(w, Placement)]
  , -- | The workspace a window was on when it stopped being listed, newest
    -- first. macOS stops answering Accessibility queries while it sleeps and
    -- while an app is still waking up, so a window can vanish from one poll and
    -- be back on the next; without this it would return as a window nad has
    -- never seen and land on the current workspace.
    --
    -- ponytail: capped at 'exiledMemory' entries rather than aged out, because
    -- 'WMState' has no clock. Windows past the cap are forgotten and come back
    -- as new ones.
    stExiled :: ![(w, Int)]
  , -- | Cleared by 'Quit', which is how the runtime knows to stop.
    stRunning :: !Bool
  }
  deriving (Eq, Show)

-- | One display showing workspace 1, until the first 'syncScreens' learns what
-- is really attached.
initialState :: Int -> [LayoutSpec] -> WMState w
initialState count layouts =
  WMState
    { stWorkspaces = [Workspace i Stack.empty | i <- [1 .. max 1 count]]
    , stVisible = [(0, 1)]
    , stScreen = 0
    , stLayouts = layouts
    , stPlaces = []
    , stExiled = []
    , stRunning = True
    }

-- | The workspace the focused display is showing.
stCurrent :: WMState w -> Int
stCurrent st = fromMaybe 1 (lookup (stScreen st) (stVisible st))

-- | The workspace a display is showing, if that display is attached.
workspaceOn :: Int -> WMState w -> Maybe Int
workspaceOn screen st = lookup screen (stVisible st)

-- | The windows a display is showing, in layout order.
stackOn :: Int -> WMState w -> Stack w
stackOn screen st = maybe Stack.empty (`stackFor` st) (workspaceOn screen st)

stackFor :: Int -> WMState w -> Stack w
stackFor n st = maybe Stack.empty wsStack (find ((== n) . wsId) (stWorkspaces st))

-- | Every workspace showing on some display.
visibleWorkspaces :: WMState w -> [Int]
visibleWorkspaces = map snd . stVisible

-- | Bind the attached displays to workspaces.
--
-- A display keeps the workspace it was showing. A display nad has not seen —
-- one just plugged in, or the first call after start-up — gets the lowest
-- workspace no other display has taken, so two screens never show the same
-- one. A display that has gone loses its binding, and the focused display
-- falls back to the first attached one.
--
-- An empty screen list is ignored rather than acted on: macOS reports no
-- displays while the machine sleeps, and throwing the bindings away there
-- would shuffle every workspace on wake.
syncScreens :: [Int] -> WMState w -> WMState w
syncScreens [] st = st
syncScreens attached@(first : _) st =
  st
    { stVisible = bound
    , stScreen = if stScreen st `elem` attached then stScreen st else first
    }
  where
    ids = map wsId (stWorkspaces st)
    bound = foldl' bind [] attached

    bind acc screen = acc <> [(screen, pick acc (lookup screen (stVisible st)))]

    -- More displays than workspaces is the only way the fallback runs out; two
    -- screens then share a workspace, which is odd but not broken.
    pick acc kept = case kept of
      Just n | free acc n -> n
      _ -> fromMaybe (headOr 1 ids) (find (free acc) ids)

    free acc n = n `elem` ids && n `notElem` map snd acc
    headOr d xs = case xs of (x : _) -> x; [] -> d

-- | How many vanished windows to remember the workspace of. A sleeping machine
-- has to come back with all of them still in memory, so this is well past any
-- plausible window count.
exiledMemory :: Int
exiledMemory = 256

currentStack :: WMState w -> Stack w
currentStack st = stackFor (stCurrent st) st

-- | Falls back to a plain Tall if a user configured no layouts at all, rather
-- than leaving the screen untiled with no explanation.
currentLayout :: WMState w -> LayoutSpec
currentLayout st = case stLayouts st of
  (l : _) -> l
  [] -> LayoutSpec Tall 0.55 1 8

-- | What the user decided about a window, as far as they decided anything.
placeOf :: Eq w => WMState w -> w -> Placement
placeOf st w = fromMaybe noPlacement (lookup w (stPlaces st))

-- | Grow or shrink one window by a fraction of the screen, leaving its origin
-- to whoever was deciding it before.
--
-- Clamped so a held-down key can neither grow a window past its screen nor
-- shrink it to something the user can no longer find.
resize :: Eq w => w -> (Double, Double) -> WMState w -> WMState w
resize w (dw, dh) st = place w (old {placeSize = Just resized}) st
  where
    old = placeOf st w
    (fw, fh) = fromMaybe (1, 1) (placeSize old)
    resized = (clamp 0.2 1 (fw + dw), clamp 0.2 1 (fh + dh))

-- | Record where the user put a window. Clamped to keep it on the screen and
-- big enough to grab, however far the mouse went.
place :: Eq w => w -> Placement -> WMState w -> WMState w
place w p st =
  st {stPlaces = (w, clamped) : filter ((/= w) . fst) (stPlaces st)}
  where
    clamped =
      Placement
        { placeOrigin = fmap (both (clamp 0 0.9)) (placeOrigin p)
        , placeSize = fmap (both (clamp 0.2 1)) (placeSize p)
        }
    both f (a, b) = (f a, f b)

focusedWindow :: WMState w -> Maybe w
focusedWindow = stackFocus . currentStack

workspaceOf :: Eq w => w -> WMState w -> Maybe Int
workspaceOf w st = wsId <$> find (elem w . stackItems . wsStack) (stWorkspaces st)

-- | Fold the live window list into the state.
--
-- Windows nad has never seen join the workspace @assign@ names, or the current
-- one when it names nothing; windows that have gone are dropped from wherever
-- they were, but their workspace is remembered in 'stExiled' so that one
-- coming back goes back to it. A window on another workspace stays there —
-- that is the whole point of workspaces.
--
-- @assign@ is a function because 'WMState' knows nothing about its windows
-- beyond their identity: the app name a rule matches on lives in the runtime.
--
-- ponytail: only ever consulted for windows nad has not seen before. Applying
-- it on every poll would drag a window the user moved by hand straight back.
syncWindows :: Eq w => (w -> Maybe Int) -> [w] -> WMState w -> WMState w
syncWindows assign live st =
  st
    { stWorkspaces = map syncOne (stWorkspaces st)
    , -- A placement outlives nothing: drop it with its window, or a long
      -- session accumulates entries for windows that closed hours ago.
      stPlaces = [entry | entry <- stPlaces st, fst entry `elem` live]
    , stExiled = take exiledMemory (gone <> [e | e <- stExiled st, fst e `notElem` live])
    }
  where
    known = concatMap (stackItems . wsStack) (stWorkspaces st)
    unseen = filter (`notElem` known) live
    gone =
      [ (w, wsId ws)
      | ws <- stWorkspaces st
      , w <- stackItems (wsStack ws)
      , w `notElem` live
      ]

    -- Where a window nad has not got on a workspace belongs. A window it has
    -- seen before goes back where it was: the rule only ever decides where a
    -- window opens. A workspace that does not exist is ignored rather than
    -- losing the window somewhere the user cannot switch to.
    target w = case remembered w of
      Just n | any ((== n) . wsId) (stWorkspaces st) -> n
      _ -> stCurrent st

    remembered w = case lookup w (stExiled st) of
      Just n -> Just n
      Nothing -> assign w

    syncOne ws =
      ws {wsStack = Stack.sync (mine ws <> arriving ws) (wsStack ws)}

    arriving ws = filter ((== wsId ws) . target) unseen

    -- A workspace only ever keeps the live windows that already belong to it.
    mine ws = filter (`elem` stackItems (wsStack ws)) live

apply :: Eq w => Action -> WMState w -> WMState w
apply action st = case action of
  Focus Next -> onStack Stack.focusNext
  Focus Prev -> onStack Stack.focusPrev
  Swap Next -> onStack Stack.swapNext
  Swap Prev -> onStack Stack.swapPrev
  SwapMaster -> onStack Stack.swapMaster
  CycleLayout -> st {stLayouts = nextLayout (stLayouts st)}
  ResizeMaster delta -> onLayout $ \l ->
    l {specMasterRatio = clamp 0.1 0.9 (specMasterRatio l + delta)}
  IncMaster delta -> onLayout $ \l ->
    l {specMasterCount = max 1 (specMasterCount l + delta)}
  ResizeWindow dw dh -> maybe st (\w -> resize w (dw, dh) st) (focusedWindow st)
  View n
    | not (validWorkspace n) -> st
    -- Already here: nothing to do. Showing on another display: the two
    -- displays trade workspaces, so the one the user asked for comes to them
    -- and the other display is left showing something rather than nothing.
    -- This is xmonad's greedyView.
    | otherwise -> case find ((== n) . snd) (stVisible st) of
        Just (holder, _)
          | holder == stScreen st -> st
          | otherwise -> setVisible [(holder, stCurrent st), (stScreen st, n)]
        Nothing -> setVisible [(stScreen st, n)]
  MoveToWorkspace n
    | validWorkspace n -> moveFocused n
    | otherwise -> st
  -- Sending a window to a display means sending it to the workspace that
  -- display is showing; the layout pass then puts it there. Nothing here
  -- touches a window's coordinates.
  MoveToScreen dir -> maybe st moveFocused (neighbourWorkspace dir)
  FocusScreen dir -> maybe st (\s -> st {stScreen = s}) (neighbour dir)
  Retile -> st
  Quit -> st {stRunning = False}
  where
    validWorkspace n = any ((== n) . wsId) (stWorkspaces st)

    setVisible changes =
      st {stVisible = [(s, fromMaybe w (lookup s changes)) | (s, w) <- stVisible st]}

    -- The next or previous attached display, wrapping. Nothing when only one
    -- is attached, so both screen actions are no-ops on a single display.
    neighbour dir
      | length (stVisible st) < 2 = Nothing
      | otherwise = do
          let screens = map fst (stVisible st)
          i <- elemIndex (stScreen st) screens
          let step = case dir of Next -> 1; Prev -> -1
          pure (screens !! ((i + step) `mod` length screens))

    neighbourWorkspace dir = neighbour dir >>= (`workspaceOn` st)

    onStack f =
      st
        { stWorkspaces =
            [ if wsId ws == stCurrent st then ws {wsStack = f (wsStack ws)} else ws
            | ws <- stWorkspaces st
            ]
        }

    onLayout f = st {stLayouts = case stLayouts st of [] -> []; (l : ls) -> f l : ls}

    moveFocused target = case focusedWindow st of
      Nothing -> st
      Just w ->
        st
          { stWorkspaces =
              [ case () of
                  _
                    | wsId ws == stCurrent st -> ws {wsStack = Stack.remove w (wsStack ws)}
                    | wsId ws == target -> ws {wsStack = Stack.insert w (wsStack ws)}
                    | otherwise -> ws
              | ws <- stWorkspaces st
              ]
          }

clamp :: Double -> Double -> Double -> Double
clamp lo hi = max lo . min hi
