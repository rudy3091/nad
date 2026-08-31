-- | Layouts: pure functions from an area and an ordered window list to
-- placements. No macOS involved, so these are fully testable.
--
-- The order of the input list is the order the layout arranges: head is the
-- master window. The output preserves that order.
module Nad.Core.Layout
  ( Layout (..)
  , LayoutSpec (..)
  , defaultLayouts
  , layoutName
  , arrange
  , nextLayout
  ) where

import Nad.Types.Geometry (Rect (..), inset)

-- | How windows are arranged in an area.
data Layout
  = -- | One master column on the left holding @masterCount@ windows, the rest
    -- stacked in a column on the right. @masterRatio@ is the master column's
    -- share of the width.
    Tall
  | -- | Every window fills the whole area, so only the raised one is visible.
    Full
  | -- | Windows cascade with a fixed offset, leaving every title bar reachable.
    Stacking
  deriving (Eq, Show)

-- | A layout plus the knobs the user can turn at runtime.
data LayoutSpec = LayoutSpec
  { specLayout :: !Layout
  , specMasterRatio :: !Double
  , specMasterCount :: !Int
  -- | Space between tiles and around the edge of the area.
  , specGap :: !Double
  }
  deriving (Eq, Show)

defaultLayouts :: [LayoutSpec]
defaultLayouts =
  [ LayoutSpec Tall 0.55 1 8
  , LayoutSpec Full 0.55 1 0
  , LayoutSpec Stacking 0.55 1 8
  ]

layoutName :: LayoutSpec -> String
layoutName = show . specLayout

-- | Advance to the next layout, wrapping. An empty list is left alone.
nextLayout :: [LayoutSpec] -> [LayoutSpec]
nextLayout [] = []
nextLayout (x : xs) = xs <> [x]

-- | Which window ends up on top is the caller's job: it raises the focused one
-- after applying these frames.
arrange :: LayoutSpec -> Rect -> [w] -> [(w, Rect)]
arrange spec area windows = case specLayout spec of
  Full -> [(w, gapped area) | w <- windows]
  Stacking -> zipWith cascade [0 ..] windows
  Tall -> tall spec area windows
  where
    gapped = inset (specGap spec)

    -- Enough to leave the title bar of every window below visible.
    cascadeStep = 28

    cascade :: Int -> w -> (w, Rect)
    cascade i w = (w, gapped area {rectX = rectX area + off, rectY = rectY area + off, rectW = rectW area - off, rectH = rectH area - off})
      where
        -- Wrap back to the top-left rather than marching a long stack off screen.
        off = fromIntegral (i `mod` 6) * cascadeStep

tall :: LayoutSpec -> Rect -> [w] -> [(w, Rect)]
tall spec area windows
  | null windows = []
  | null stack = column area masters
  | null masters = column area stack
  | otherwise = column masterArea masters <> column stackArea stack
  where
    (masters, stack) = splitAt (max 1 (specMasterCount spec)) windows

    -- Clamped so a user hammering the resize key cannot make a column vanish.
    ratio = min 0.9 (max 0.1 (specMasterRatio spec))
    masterWidth = rectW area * ratio
    masterArea = area {rectW = masterWidth}
    stackArea = area {rectX = rectX area + masterWidth, rectW = rectW area - masterWidth}

    -- Split a rect into equal horizontal bands, one per window.
    column :: Rect -> [w] -> [(w, Rect)]
    column r ws = zipWith band [0 ..] ws
      where
        n = length ws
        height = rectH r / fromIntegral n
        band :: Int -> w -> (w, Rect)
        band i w =
          ( w
          , inset
              (specGap spec)
              r {rectY = rectY r + fromIntegral i * height, rectH = height}
          )
