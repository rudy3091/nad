-- | Global hotkeys via a CGEventTap, and the main thread's run loop.
module Nad.Platform.Hotkey
  ( startHotkeys
  , runEventLoop
  , stopEventLoop
  , systemHotkeys
  , claimSystemHotkeys
  , releaseSystemHotkeys
  , releaseStaleHotkeys
  , secureInputHolder
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.Maybe (catMaybes)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import System.Directory (doesFileExist, removeFile)
import Text.Read (readMaybe)

import Nad.Paths (claimedHotkeysPath)
import Nad.Platform.FFI
import Nad.Platform.Marshal (peekOwnedCString)
import Nad.Types.Key (KeyCombo (..), KeyCode (..), conflicting, unpackModifiers)

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

-- | The application holding secure keyboard entry, if any.
--
-- While one does, the WindowServer sends no key events to event taps, so every
-- nad binding is dead for as long as that application is focused. Nothing can
-- be done about it from here — but saying which application it is turns a
-- silently broken keyboard into a one-line explanation.
-- The name is best effort: macOS only records the holder's pid when it is the
-- active application, so a generic label has to do the rest of the time.
secureInputHolder :: IO (Maybe String)
secureInputHolder = do
  enabled <- (/= 0) <$> c_secure_input_enabled
  if not enabled
    then pure Nothing
    else do
      pid <- fromIntegral <$> c_secure_input_pid :: IO Int
      name <-
        if pid > 0
          then peekOwnedCString =<< c_app_name (fromIntegral pid)
          else pure ""
      pure (Just (if null name then "An application" else name))

-- | The system shortcuts that are currently switched on, with what they answer
-- to. Ids are sparse, so this walks the whole table.
systemHotkeys :: IO [(Int, KeyCombo)]
systemHotkeys = catMaybes <$> mapM readOne [0 .. symbolicHotkeyMax - 1]
  where
    readOne hotkeyId =
      alloca $ \codePtr -> alloca $ \modsPtr -> do
        found <- c_symbolic_hotkey_get (fromIntegral hotkeyId) codePtr modsPtr
        enabled <- c_symbolic_hotkey_enabled (fromIntegral hotkeyId)
        if found /= 0 || enabled == 0
          then pure Nothing
          else do
            code <- peek codePtr
            mods <- peek modsPtr
            pure (Just (hotkeyId, KeyCombo (unpackModifiers mods) (KeyCode code)))

-- | Switch off every system shortcut that would shadow one of these bindings,
-- and return the ids so they can be switched back on.
--
-- The ids are also written to disk first: a nad that is killed outright never
-- gets to restore anything, and a user whose Spotlight silently stopped working
-- has no way to guess why. 'releaseStaleHotkeys' picks that record up.
claimSystemHotkeys :: [KeyCombo] -> IO [Int]
claimSystemHotkeys wanted = do
  taken <- conflicting wanted <$> systemHotkeys
  if null taken
    then pure []
    else do
      path <- claimedHotkeysPath
      writeFile path (unlines (map show taken))
      forM_ taken $ \hotkeyId -> c_symbolic_hotkey_set_enabled (fromIntegral hotkeyId) 0
      pure taken

releaseSystemHotkeys :: [Int] -> IO ()
releaseSystemHotkeys taken = do
  forM_ taken $ \hotkeyId -> c_symbolic_hotkey_set_enabled (fromIntegral hotkeyId) 1
  path <- claimedHotkeysPath
  present <- doesFileExist path
  if present then removeFile path else pure ()

-- | Undo a previous run that did not get to clean up after itself. Safe to call
-- when there is nothing to undo.
releaseStaleHotkeys :: IO ()
releaseStaleHotkeys = do
  path <- claimedHotkeysPath
  present <- doesFileExist path
  if not present
    then pure ()
    else do
      -- A corrupt or unreadable record must not stop nad from starting.
      contents <- try (readFile path) :: IO (Either SomeException String)
      case contents of
        Left _ -> pure ()
        Right text -> releaseSystemHotkeys (catMaybes (map readMaybe (lines text)))
