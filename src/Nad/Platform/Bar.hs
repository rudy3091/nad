-- | Putting 'BarContent' on screen, one borderless window per display.
module Nad.Platform.Bar
  ( Bar (..)
  , initApp
  , createBars
  , updateBar
  , destroyBars
  ) where

import Data.Maybe (mapMaybe)
import Foreign.C.String (withCString)
import Foreign.C.Types (CDouble (..))

import Nad.Bar.Segment (BarContent (..), encodeSegments)
import Nad.Platform.FFI
import Nad.Types.Config (BarConfig (..), BarPosition (..))
import Nad.Types.Geometry (Rect (..), axToCocoaY)
import Nad.Types.Window (ScreenInfo (..))

data Bar = Bar
  { barHandle :: !Int
  , barScreen :: !Int
  }
  deriving (Eq, Show)

-- | Must run on the main thread, before the run loop starts.
initApp :: IO ()
initApp = c_app_init

-- | One bar per screen, positioned in the strip 'Nad.Types.Config.reserveBar'
-- keeps free.
--
-- ponytail: bars are created once. Plugging in a display mid-session needs a
-- restart until there is a display-reconfiguration callback to hang this off.
createBars :: BarConfig -> Double -> [ScreenInfo] -> IO [Bar]
createBars cfg mainHeight screens = mapMaybe id <$> mapM create screens
  where
    create screen = do
      -- Callers pass screens from before 'reserveBar' has taken this strip away.
      let rect = barRect cfg mainHeight screen
      handle <-
        withCString (barBackground cfg) $ \bg ->
          withCString (barForeground cfg) $ \fg ->
            withCString (barFont cfg) $ \font ->
              c_bar_create
                (CDouble (rectX rect))
                (CDouble (rectY rect))
                (CDouble (rectW rect))
                (CDouble (rectH rect))
                bg
                fg
                font
                (CDouble (barFontSize cfg))
      pure $
        if handle < 0
          then Nothing
          else Just (Bar (fromIntegral handle) (screenIndex screen))

updateBar :: Bar -> BarContent -> IO ()
updateBar bar content =
  withCString (encodeSegments (barLeft content)) $ \l ->
    withCString (encodeSegments (barCenter content)) $ \c ->
      withCString (encodeSegments (barRight content)) $ \r ->
        c_bar_set (fromIntegral (barHandle bar)) l c r

destroyBars :: IO ()
destroyBars = c_bar_destroy_all

-- | The bar's own frame, in Cocoa coordinates, spanning the width of a screen.
--
-- A top bar goes at the very top of the display and is drawn over the menu bar,
-- which is also what puts it level with the notch strip on a MacBook rather
-- than the notch's height below it. A bottom bar stays inside the usable area,
-- so it sits above the Dock instead of under it.
barRect :: BarConfig -> Double -> ScreenInfo -> Rect
barRect cfg mainHeight screen =
  Rect
    { rectX = rectX area
    , rectY = axToCocoaY mainHeight (barHeight cfg) axY
    , rectW = rectW area
    , rectH = barHeight cfg
    }
  where
    (area, axY) = case barPosition cfg of
      Top -> (screenFrame screen, rectY (screenFrame screen))
      Bottom ->
        let u = screenUsable screen
         in (u, rectY u + rectH u - barHeight cfg)
