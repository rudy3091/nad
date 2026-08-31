-- | What the status bar says. Pure: 'BarContent' is a description, and
-- "Nad.Platform.Bar" is what puts it on screen.
module Nad.Bar.Segment
  ( Segment (..)
  , BarContent (..)
  , BarState (..)
  , seg
  , colored
  , emptyContent
  , encodeSegments
  , defaultRender
  ) where

import Data.List (intercalate)

-- | A run of text with optional colours. @Nothing@ means the bar's default.
data Segment = Segment
  { segText :: String
  , segFg :: Maybe String
  -- ^ @\"#RRGGBB\"@
  , segBg :: Maybe String
  }
  deriving (Eq, Show)

-- | The three regions of the bar.
data BarContent = BarContent
  { barLeft :: [Segment]
  , barCenter :: [Segment]
  , barRight :: [Segment]
  }
  deriving (Eq, Show)

-- | Everything a renderer is given. Flat on purpose: a user's render function
-- should not have to know about nad's internal state types.
data BarState = BarState
  { bsWorkspaces :: [(Int, Bool, Int)]
  -- ^ Workspace id, whether it is showing, how many windows it holds.
  , bsLayout :: String
  , bsFocused :: String
  -- ^ Title of the focused window, empty when there is none.
  , bsScreen :: Int
  -- ^ Which screen this bar is on, so a renderer can say so.
  , bsClock :: String
  }
  deriving (Eq, Show)

seg :: String -> Segment
seg t = Segment t Nothing Nothing

colored :: String -> String -> Segment
colored fg t = Segment t (Just fg) Nothing

emptyContent :: BarContent
emptyContent = BarContent [] [] []

-- | Wire format for "Nad.Platform.Bar": records joined by @\\x1e@, fields
-- within a record by @\\x1f@. Matches the parser in @cbits/nad_bar.m@.
encodeSegments :: [Segment] -> String
encodeSegments = intercalate "\x1e" . map encodeOne
  where
    encodeOne s =
      intercalate "\x1f" [maybe "" id (segFg s), maybe "" id (segBg s), sanitize (segText s)]
    -- Text carrying a separator would corrupt the record boundaries, and window
    -- titles are arbitrary user data.
    sanitize = map (\c -> if c == '\x1e' || c == '\x1f' then ' ' else c)

-- | The bar a user gets without configuring one: workspaces on the left, the
-- focused window in the middle, layout and clock on the right.
defaultRender :: BarState -> BarContent
defaultRender st =
  BarContent
    { barLeft = map workspace (bsWorkspaces st)
    , barCenter = [seg (bsFocused st)]
    , barRight = [seg (bsLayout st), seg "  ", colored "#88aaff" (bsClock st)]
    }
  where
    workspace (wid, current, count)
      | current = Segment (" " <> show wid <> " ") (Just "#1b1b1b") (Just "#88aaff")
      | count > 0 = colored "#dddddd" (" " <> show wid <> " ")
      -- An empty workspace is still worth showing, just quietly.
      | otherwise = colored "#555555" (" " <> show wid <> " ")
