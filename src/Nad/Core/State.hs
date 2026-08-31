-- | The window manager's state and how actions change it. Pure: the runtime
-- takes the resulting state and makes the screen match it.
module Nad.Core.State
  ( WMState (..)
  , Workspace (..)
  , initialState
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

import Data.List (find)
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
  , stCurrent :: !Int
  -- | Head is the layout in use; 'CycleLayout' rotates the list.
  , stLayouts :: ![LayoutSpec]
  -- | Per-window placement overrides, as fractions of the screen. Only windows
  -- the user has actually resized or dragged appear here, and only
  -- 'Nad.Core.Layout.Stacking' reads them.
  , stPlaces :: ![(w, Placement)]
  , -- | Cleared by 'Quit', which is how the runtime knows to stop.
    stRunning :: !Bool
  }
  deriving (Eq, Show)

initialState :: Int -> [LayoutSpec] -> WMState w
initialState count layouts =
  WMState
    { stWorkspaces = [Workspace i Stack.empty | i <- [1 .. max 1 count]]
    , stCurrent = 1
    , stLayouts = layouts
    , stPlaces = []
    , stRunning = True
    }

currentStack :: WMState w -> Stack w
currentStack st =
  maybe Stack.empty wsStack (find ((== stCurrent st) . wsId) (stWorkspaces st))

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
-- Windows nad has never seen join the current workspace; windows that have gone
-- are dropped from wherever they were. A window on another workspace stays
-- there — that is the whole point of workspaces.
syncWindows :: Eq w => [w] -> WMState w -> WMState w
syncWindows live st =
  st
    { stWorkspaces = map syncOne (stWorkspaces st)
    , -- A placement outlives nothing: drop it with its window, or a long
      -- session accumulates entries for windows that closed hours ago.
      stPlaces = [entry | entry <- stPlaces st, fst entry `elem` live]
    }
  where
    known = concatMap (stackItems . wsStack) (stWorkspaces st)
    unseen = filter (`notElem` known) live

    syncOne ws
      | wsId ws == stCurrent st = ws {wsStack = Stack.sync (mine ws <> unseen) (wsStack ws)}
      | otherwise = ws {wsStack = Stack.sync (mine ws) (wsStack ws)}

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
    | validWorkspace n -> st {stCurrent = n}
    | otherwise -> st
  MoveToWorkspace n
    | validWorkspace n -> moveFocused n
    | otherwise -> st
  -- Both need to know about screens, which only the runtime does.
  MoveToScreen _ -> st
  Retile -> st
  Quit -> st {stRunning = False}
  where
    validWorkspace n = any ((== n) . wsId) (stWorkspaces st)

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
