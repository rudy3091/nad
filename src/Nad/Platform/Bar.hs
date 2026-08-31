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
      -- Placed against the usable area, not the whole display, so the bar sits
      -- below the menu bar rather than under it. Callers pass screens from
      -- before 'reserveBar' has taken this strip away.
      let rect = barRect cfg mainHeight (screenUsable screen)
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
barRect :: BarConfig -> Double -> Rect -> Rect
barRect cfg mainHeight screen =
  Rect
    { rectX = rectX screen
    , rectY = axToCocoaY mainHeight (barHeight cfg) axY
    , rectW = rectW screen
    , rectH = barHeight cfg
    }
  where
    axY = case barPosition cfg of
      Top -> rectY screen
      Bottom -> rectY screen + rectH screen - barHeight cfg
