-- | Rectangles and the two coordinate systems macOS makes us care about.
--
-- Everything in nad is kept in __AX coordinates__: origin at the top-left of
-- the main display, y growing downwards. Cocoa (@NSScreen@, @NSWindow@) uses
-- the bottom-left of the main display with y growing upwards, so any value
-- crossing that boundary goes through 'axToCocoaY' / 'cocoaToAxY'.
module Nad.Types.Geometry
  ( Rect (..)
  , Point (..)
  , rectRight
  , rectBottom
  , center
  , contains
  , containsPoint
  , intersects
  , inset
  , axToCocoaY
  , cocoaToAxY
  ) where

data Point = Point
  { pointX :: !Double
  , pointY :: !Double
  }
  deriving (Eq, Show)

data Rect = Rect
  { rectX :: !Double
  , rectY :: !Double
  , rectW :: !Double
  , rectH :: !Double
  }
  deriving (Eq, Show)

rectRight :: Rect -> Double
rectRight r = rectX r + rectW r

rectBottom :: Rect -> Double
rectBottom r = rectY r + rectH r

center :: Rect -> Point
center r = Point (rectX r + rectW r / 2) (rectY r + rectH r / 2)

-- | Half-open on the far edges, so a point on a boundary belongs to exactly one
-- of two adjacent rectangles.
containsPoint :: Rect -> Point -> Bool
containsPoint r p =
  pointX p >= rectX r
    && pointY p >= rectY r
    && pointX p < rectRight r
    && pointY p < rectBottom r

-- | Shrink a rectangle by a gap on every side. Never returns a negative extent.
inset :: Double -> Rect -> Rect
inset gap r =
  Rect
    { rectX = rectX r + gap
    , rectY = rectY r + gap
    , rectW = max 0 (rectW r - 2 * gap)
    , rectH = max 0 (rectH r - 2 * gap)
    }

-- | Is @inner@ fully inside @outer@?
contains :: Rect -> Rect -> Bool
contains outer inner =
  rectX inner >= rectX outer
    && rectY inner >= rectY outer
    && rectRight inner <= rectRight outer
    && rectBottom inner <= rectBottom outer

-- | Do the two rectangles overlap in a non-empty area? Shared edges do not
-- count, so tiled neighbours are not reported as overlapping.
intersects :: Rect -> Rect -> Bool
intersects a b =
  rectX a < rectRight b
    && rectX b < rectRight a
    && rectY a < rectBottom b
    && rectY b < rectBottom a

-- | Flip a y coordinate between the two systems. Both directions are the same
-- computation; they are named separately so call sites read unambiguously.
--
-- @mainHeight@ is the height of the main display, @h@ the height of the thing
-- being placed (0 for a bare point).
axToCocoaY :: Double -> Double -> Double -> Double
axToCocoaY mainHeight h y = mainHeight - y - h

cocoaToAxY :: Double -> Double -> Double -> Double
cocoaToAxY = axToCocoaY
