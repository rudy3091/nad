-- | Reading and moving windows through the Accessibility API.
module Nad.Platform.Window
  ( isTrusted
  , requestTrust
  , listWindows
  , setWindowFrame
  , focusWindow
  ) where

import Control.Exception (bracket)
import Data.Maybe (fromMaybe)
import Foreign.C.Types (CDouble (..))
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)

import Nad.Platform.FFI
import Nad.Platform.Marshal (peekOwnedCString, withRectOut)
import Nad.Types.Geometry (Rect (..))
import Nad.Types.Window (WindowInfo (..), WindowRef (..), withWindowRef)

-- | Does this process hold the Accessibility permission?
isTrusted :: IO Bool
isTrusted = (/= 0) <$> c_ax_trusted 0

-- | Same as 'isTrusted', but shows the system permission dialog when missing.
requestTrust :: IO Bool
requestTrust = (/= 0) <$> c_ax_trusted 1

-- | The standard windows currently open, in the order macOS reports them.
--
-- Each handle is retained and released when the 'WindowInfo' is collected, so
-- the list stays usable across events. The window /metadata/ is a snapshot
-- though: re-list to see moves and title changes.
--
-- ponytail: re-listing costs one AX round-trip per app. Switch to AXObserver
-- notifications if that shows up in latency.
listWindows :: IO [WindowInfo]
listWindows =
  alloca $ \arrayPtr -> do
    count <- fromIntegral <$> c_ax_list_windows arrayPtr
    if count <= 0
      then pure []
      else
        -- The array is ours to free; the handles inside outlive it.
        bracket (peek arrayPtr) c_free $ \handles ->
          if handles == nullPtr
            then pure []
            else peekArray count handles >>= mapM readWindow

-- | Move and resize a window. Returns 'False' when the app refused, which
-- happens for windows that are not resizable.
setWindowFrame :: WindowRef -> Rect -> IO Bool
setWindowFrame ref r =
  withWindowRef ref $ \handle ->
    (== 0)
      <$> c_ax_set_window_frame
        handle
        (CDouble (rectX r))
        (CDouble (rectY r))
        (CDouble (rectW r))
        (CDouble (rectH r))

-- | Raise a window and bring its application forward.
focusWindow :: WindowRef -> IO Bool
focusWindow ref = withWindowRef ref $ \handle -> (== 0) <$> c_ax_focus_window handle

readWindow :: NadWindow -> IO WindowInfo
readWindow handle = do
  app <- peekOwnedCString =<< c_ax_window_app handle
  title <- peekOwnedCString =<< c_ax_window_title handle
  pid <- fromIntegral <$> c_ax_window_pid handle
  frame <- fromMaybe (Rect 0 0 0 0) <$> withRectOut (c_ax_window_frame handle)
  ref <- WindowRef <$> newForeignPtr p_ax_release handle
  pure
    WindowInfo
      { winRef = ref
      , winApp = app
      , winTitle = title
      , winPid = pid
      , winFrame = frame
      }
