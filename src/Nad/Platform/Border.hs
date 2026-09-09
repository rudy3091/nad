-- | The outline nad draws around the focused window.
--
-- macOS gives no way to decorate another application's window, so this is a
-- transparent overlay window of nad's own, moved to sit on the focused
-- window's edge.
module Nad.Platform.Border
  ( showBorder
  , hideBorder
  , borderRect
  ) where

import Foreign.C.String (withCString)
import Foreign.C.Types (CDouble (..))

import Nad.Platform.FFI (c_border_hide, c_border_set)
import Nad.Types.Config (BorderConfig (..))
import Nad.Types.Geometry (Rect (..), axToCocoaY)

-- | Outline a window, given the frame the layout gave it in AX coordinates.
-- A disabled border hides instead, so toggling it off in the config takes
-- effect without a special case at the call site.
showBorder :: BorderConfig -> Double -> Rect -> IO ()
showBorder cfg mainHeight target
  | not (borderEnabled cfg) || borderWidth cfg <= 0 = hideBorder
  | otherwise =
      withCString (borderColor cfg) $ \color ->
        c_border_set
          (CDouble (rectX r))
          (CDouble (axToCocoaY mainHeight (rectH r) (rectY r)))
          (CDouble (rectW r))
          (CDouble (rectH r))
          color
          (CDouble (borderWidth cfg))
  where
    r = borderRect (borderWidth cfg) target

hideBorder :: IO ()
hideBorder = c_border_hide

-- | The overlay's own frame: the window's, grown by the outline's thickness on
-- every side. The shim strokes inwards, so this keeps the outline outside the
-- window rather than over its first few pixels.
borderRect :: Double -> Rect -> Rect
borderRect thickness r =
  Rect
    { rectX = rectX r - thickness
    , rectY = rectY r - thickness
    , rectW = rectW r + 2 * thickness
    , rectH = rectH r + 2 * thickness
    }
