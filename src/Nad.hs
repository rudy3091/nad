-- | Public entry point.
--
-- A user's @~\/.nad\/nad.hs@ imports this module, adjusts 'defaultConfig' and
-- passes it to 'nadWith'. Everything needed to write such a config is
-- re-exported here, so one import is enough.
module Nad
  ( nad
  , nadWith
  , module Nad.Bar.Segment
  , module Nad.Core.Action
  , module Nad.Core.Layout
  , module Nad.Types.Config
  , module Nad.Types.Geometry
  , module Nad.Types.Window
  ) where

import System.Environment (getArgs)

import Nad.Bar.Segment
import Nad.Cli (parseCommand, runCommand)
import Nad.Core.Action
import Nad.Core.Layout
import Nad.Types.Config
import Nad.Types.Geometry
import Nad.Types.Window

-- | Run nad with the configuration it ships with.
nad :: IO ()
nad = nadWith defaultConfig

nadWith :: Config -> IO ()
nadWith cfg = getArgs >>= runCommand cfg . parseCommand
