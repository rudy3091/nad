-- | Global hotkeys via a CGEventTap, and the main thread's run loop.
module Nad.Platform.Hotkey
  ( startHotkeys
  , runEventLoop
  , stopEventLoop
  ) where

import Foreign.C.Types (CInt)

import Nad.Platform.FFI
import Nad.Types.Key (KeyCombo (..), KeyCode (..), unpackModifiers)

-- | Install a global key handler. The handler returns 'True' for a key nad
-- claims, which stops it reaching the focused app.
--
-- It runs on the main thread inside the event tap, so it must be quick:
-- anything slower than a few milliseconds and macOS disables the tap. Queue the
-- work instead of doing it here.
--
-- 'False' means the tap could not be created, which in practice means the
-- Input Monitoring permission is missing.
startHotkeys :: (KeyCombo -> IO Bool) -> IO Bool
startHotkeys handler = do
  callback <- mkKeyHandler (\code mods -> toCInt <$> handler (toCombo code mods))
  (== 0) <$> c_hotkey_start callback
  where
    toCombo code mods = KeyCombo (unpackModifiers mods) (KeyCode code)
    toCInt consumed = if consumed then 1 else 0 :: CInt

-- | Hand the calling thread to CFRunLoop. Must be the main thread, and never
-- returns until 'stopEventLoop'.
runEventLoop :: IO ()
runEventLoop = c_run_loop

stopEventLoop :: IO ()
stopEventLoop = c_stop_run_loop
