-- | Everything nad can be asked to do.
--
-- The name table below is the single definition of an action's spelling. Key
-- bindings and the @nad msg@ command line both go through it, so adding an
-- action here makes it reachable from both without further work.
module Nad.Core.Action
  ( Action (..)
  , Direction (..)
  , parseAction
  , showAction
  , actionNames
  ) where

import Data.List (find)
import Text.Read (readMaybe)

data Direction = Next | Prev
  deriving (Eq, Show)

data Action
  = -- | Move focus along the window order.
    Focus Direction
  | -- | Move the focused window along the window order.
    Swap Direction
  | -- | Promote the focused window to master, or demote it back.
    SwapMaster
  | -- | Grow or shrink the master area.
    ResizeMaster Double
  | -- | Change how many windows share the master area.
    IncMaster Int
  | -- | Resize the focused window itself, by a fraction of its screen. Only
    -- 'Nad.Core.Layout.Stacking' can honour this; the tiled layouts decide a
    -- window's size from its neighbours.
    ResizeWindow Double Double
  | CycleLayout
  | -- | Show a workspace on the focused screen.
    View Int
  | -- | Send the focused window to a workspace.
    MoveToWorkspace Int
  | -- | Send the focused window to the next or previous screen.
    MoveToScreen Direction
  | -- | Re-apply the current layout, e.g. after an app moved its own window.
    Retile
  | Quit
  deriving (Eq, Show)

-- | Parameterless actions, by name.
actionNames :: [(String, Action)]
actionNames =
  [ ("focus-next", Focus Next)
  , ("focus-prev", Focus Prev)
  , ("swap-next", Swap Next)
  , ("swap-prev", Swap Prev)
  , ("swap-master", SwapMaster)
  , ("shrink-master", ResizeMaster (-0.05))
  , ("expand-master", ResizeMaster 0.05)
  , ("inc-master", IncMaster 1)
  , ("dec-master", IncMaster (-1))
  , ("window-narrower", ResizeWindow (-0.05) 0)
  , ("window-wider", ResizeWindow 0.05 0)
  , ("window-shorter", ResizeWindow 0 (-0.05))
  , ("window-taller", ResizeWindow 0 0.05)
  , ("cycle-layout", CycleLayout)
  , ("screen-next", MoveToScreen Next)
  , ("screen-prev", MoveToScreen Prev)
  , ("retile", Retile)
  , ("quit", Quit)
  ]

-- | Parse a command line: either a bare action name, or a name plus one
-- argument (@workspace 3@).
parseAction :: [String] -> Maybe Action
parseAction ws = case ws of
  [name] -> lookup name actionNames
  ["workspace", n] -> View <$> readMaybe n
  ["move-to-workspace", n] -> MoveToWorkspace <$> readMaybe n
  _ -> Nothing

showAction :: Action -> String
showAction action = case action of
  View n -> "workspace " <> show n
  MoveToWorkspace n -> "move-to-workspace " <> show n
  other -> maybe "?" fst (find ((== other) . snd) actionNames)
