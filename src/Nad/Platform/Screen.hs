-- | Display geometry, converted from Cocoa's coordinate system into the AX
-- coordinates the rest of nad uses.
module Nad.Platform.Screen
  ( listScreens
  , mainScreenHeight
  ) where

import Data.Maybe (catMaybes)
import Foreign.C.Types (CDouble (..), CInt (..))

import Nad.Platform.FFI
import Nad.Platform.Marshal (withRectOut)
import Nad.Types.Geometry (Rect (..), cocoaToAxY)
import Nad.Types.Window (ScreenInfo (..))

-- | Height of the display holding the menu bar, which is what the AX and Cocoa
-- coordinate systems are flipped around.
mainScreenHeight :: IO Double
mainScreenHeight = (\(CDouble d) -> d) <$> c_screen_main_height

-- | Screens in the order macOS reports them; index 0 holds the menu bar.
listScreens :: IO [ScreenInfo]
listScreens = do
  mainHeight <- mainScreenHeight
  count <- fromIntegral <$> c_screen_count
  catMaybes <$> mapM (readScreen mainHeight) [0 .. count - 1]

readScreen :: Double -> Int -> IO (Maybe ScreenInfo)
readScreen mainHeight index = do
  full <- frameOf 0
  visible <- frameOf 1
  pure $ do
    f <- full
    v <- visible
    pure
      ScreenInfo
        { screenIndex = index
        , screenFrame = toAx mainHeight f
        , screenUsable = toAx mainHeight v
        }
  where
    frameOf visible = withRectOut (c_screen_frame (fromIntegral index) (CInt visible))

toAx :: Double -> Rect -> Rect
toAx mainHeight r = r {rectY = cocoaToAxY mainHeight (rectH r) (rectY r)}
