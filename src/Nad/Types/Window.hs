-- | Window and screen identity.
--
-- 'WindowRef' wraps a Core Foundation handle, so this module reaches into
-- "Nad.Platform.FFI" for the two calls that define the handle's lifetime and
-- equality. Everything downstream of here — all of "Nad.Core" — stays pure.
module Nad.Types.Window
  ( WindowRef (..)
  , withWindowRef
  , WindowInfo (..)
  , ScreenInfo (..)
  , describeWindow
  , describeScreen
  , screenFor
  ) where

import Data.List (find)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafeDupablePerformIO)

import Nad.Platform.FFI (c_ax_same_window)
import Nad.Types.Geometry (Rect (..), center, containsPoint)

-- | An owned @AXUIElementRef@ for a window, released when garbage collected.
--
-- macOS exposes no public stable window id, so the element itself is the
-- identity. Two handles for the same window are usually different pointers, so
-- equality has to go through @CFEqual@ — see 'Nad.Platform.Window.sameWindow',
-- which is what the 'Eq' instance is built on.
newtype WindowRef = WindowRef (ForeignPtr ())

-- | @CFEqual@ is a pure comparison of two live handles; the IO is an artefact of
-- the FFI, not an effect, so it is safe to look at from pure code.
instance Eq WindowRef where
  a == b = unsafeDupablePerformIO $
    withWindowRef a $ \pa ->
      withWindowRef b $ \pb ->
        (/= 0) <$> c_ax_same_window pa pb

instance Show WindowRef where
  show _ = "<window>"

withWindowRef :: WindowRef -> (Ptr () -> IO a) -> IO a
withWindowRef (WindowRef fp) = withForeignPtr fp

data WindowInfo = WindowInfo
  { winRef :: !WindowRef
  , winApp :: !String
  , winTitle :: !String
  , winPid :: !Int
  , winFrame :: !Rect
  }
  deriving (Eq, Show)

data ScreenInfo = ScreenInfo
  { screenIndex :: !Int
  -- | Whole display, AX coordinates.
  , screenFrame :: !Rect
  -- | The part windows may use: menu bar and Dock excluded. Layouts tile inside
  -- this, minus the status bar once it exists.
  , screenUsable :: !Rect
  }
  deriving (Eq, Show)

-- | Which screen a window lives on, decided by its centre. A window straddling
-- two displays belongs to the one showing most of it, which is what a user
-- means by "this screen".
screenFor :: [ScreenInfo] -> Rect -> Maybe ScreenInfo
screenFor screens r = find (\s -> screenFrame s `containsPoint` center r) screens

-- | One-line human readable form, used by @nad query windows@.
describeWindow :: WindowInfo -> String
describeWindow w =
  concat [pad 22 (winApp w), pad 46 (ellipsize 44 (winTitle w)), showRect (winFrame w)]

-- | One-line human readable form, used by @nad query screens@.
describeScreen :: ScreenInfo -> String
describeScreen s =
  concat
    [ pad 8 ("[" <> show (screenIndex s) <> "]")
    , pad 24 (showRect (screenFrame s))
    , "usable " <> showRect (screenUsable s)
    ]

-- | @WxH+X+Y@, the shape people already read in window manager tools.
showRect :: Rect -> String
showRect r =
  concat
    [ int (rectW r), "x", int (rectH r), "+", int (rectX r), "+", int (rectY r) ]
  where
    int d = show (round d :: Int)

pad :: Int -> String -> String
pad n s = s <> replicate (max 1 (n - length s)) ' '

ellipsize :: Int -> String -> String
ellipsize n s = if length s > n then take (n - 1) s <> "…" else s
