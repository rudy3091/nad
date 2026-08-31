-- | Layouts: pure functions from an area and an ordered window list to
-- placements. No macOS involved, so these are fully testable.
--
-- The order of the input list is the order the layout arranges: head is the
-- master window. The output preserves that order.
module Nad.Core.Layout
  ( Layout (..)
  , LayoutSpec (..)
  , Placement (..)
  , Sizing
  , noPlacement
  , defaultLayouts
  , layoutName
  , arrange
  , arrangeWith
  , draggedPlacements
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

-- | What the user has decided about one window, in fractions of the area: where
-- its top-left corner sits, and how big it is. Either half may be missing, and
-- then the layout decides that half.
--
-- Only 'Stacking' honours this: in the tiled layouts a window's frame is
-- decided by its neighbours, not by itself.
data Placement = Placement
  { placeOrigin :: !(Maybe (Double, Double))
  , placeSize :: !(Maybe (Double, Double))
  }
  deriving (Eq, Show)

-- | The layout decides everything.
noPlacement :: Placement
noPlacement = Placement Nothing Nothing

type Sizing w = w -> Placement

-- | Which window ends up on top is the caller's job: it raises the focused one
-- after applying these frames.
arrange :: LayoutSpec -> Rect -> [w] -> [(w, Rect)]
arrange spec area = arrangeWith spec area (const noPlacement)

arrangeWith :: LayoutSpec -> Rect -> Sizing w -> [w] -> [(w, Rect)]
arrangeWith spec area sizing windows = case specLayout spec of
  Full -> [(w, gapped area) | w <- windows]
  Stacking -> zipWith cascade [0 :: Int ..] windows
  Tall -> tall spec area windows
  where
    gapped = inset (specGap spec)

    -- Enough to leave the title bar of every window below visible.
    cascadeStep = 28

    cascade i w =
      ( w
      , gapped Rect {rectX = rectX area + dx, rectY = rectY area + dy, rectW = w', rectH = h'}
      )
      where
        place = sizing w

        -- Wrap back to the top-left rather than marching a long stack off screen.
        off = fromIntegral (i `mod` 6) * cascadeStep
        (dx, dy) =
          maybe
            (off, off)
            (\(ox, oy) -> (ox * rectW area, oy * rectH area))
            (placeOrigin place)

        -- A window with no size of its own fills what is left of the area from
        -- where it sits. With one, the fraction is the whole story, so that
        -- 'draggedPlacements' can read it straight back out.
        (w', h') = case placeSize place of
          Just (fw, fh) -> (max 1 (rectW area * fw), max 1 (rectH area * fh))
          Nothing -> (max 1 (rectW area - dx), max 1 (rectH area - dy))

-- | The placements windows the user dragged with the mouse imply. @placed@ is
-- where the layout just put them, @actual@ where they really are now.
--
-- The mouse can only turn a knob the layout has, and 'Stacking' is the one
-- layout that places each window on its own. In the tiled layouts a window's
-- frame is its neighbours' business, so a drag there has nowhere to land.
--
-- A drag that resized the window keeps its origin too: grabbing the top or the
-- left edge moves both, and putting the origin back would drag the window out
-- from under the pointer. A drag that only /moved/ a window is left to the
-- layout, which is what makes this a stack of windows and not a desktop.
draggedPlacements
  :: LayoutSpec -> Rect -> (w -> Maybe Rect) -> [(w, Rect)] -> [(w, Placement)]
draggedPlacements spec area actual placed
  | specLayout spec /= Stacking = []
  | rectW area <= 0 || rectH area <= 0 = []
  | otherwise =
      [ (w, asFractions r)
      | (w, expected) <- placed
      , Just r <- [actual w]
      , resized (rectW r - rectW expected) || resized (rectH r - rectH expected)
      ]
  where
    -- ponytail: a fixed slack, because apps quantise their own size — a
    -- terminal snaps to whole character cells — and adopting that as a drag
    -- would let windows creep on their own. Above one cell, below anything a
    -- person would bother dragging.
    resized d = abs d > 12

    -- The exact inverse of 'cascade' with both halves present, gap included, so
    -- re-placing a window nad has just adopted is a no-op.
    gap = specGap spec
    asFractions r =
      Placement
        { placeOrigin =
            Just
              ( (rectX r - gap - rectX area) / rectW area
              , (rectY r - gap - rectY area) / rectH area
              )
        , placeSize =
            Just ((rectW r + 2 * gap) / rectW area, (rectH r + 2 * gap) / rectH area)
        }

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
