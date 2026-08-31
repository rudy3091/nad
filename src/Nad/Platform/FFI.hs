-- | Raw @foreign import@ declarations, 1:1 with @cbits/nad.h@. Nothing in here
-- allocates or frees on its own; "Nad.Platform.Window" owns that.
module Nad.Platform.FFI where

import Foreign.C.Types (CDouble (..), CInt (..))
import Foreign.C.String (CString)
import Foreign.Ptr (FunPtr, Ptr)
import Data.Word (Word16, Word32)

foreign import ccall unsafe "nad_ax_trusted"
  c_ax_trusted :: CInt -> IO CInt

-- | @nad_window@: an opaque @AXUIElementRef@.
type NadWindow = Ptr ()

-- | A shim-allocated array of 'NadWindow'.
type NadWindowArray = Ptr NadWindow

foreign import ccall safe "nad_ax_list_windows"
  c_ax_list_windows :: Ptr NadWindowArray -> IO CInt

foreign import ccall unsafe "nad_ax_same_window"
  c_ax_same_window :: NadWindow -> NadWindow -> IO CInt

-- | Finalizer for a window handle, used with 'Foreign.ForeignPtr.newForeignPtr'.
foreign import ccall unsafe "&nad_ax_release"
  p_ax_release :: FunPtr (NadWindow -> IO ())

foreign import ccall unsafe "nad_free"
  c_free :: Ptr a -> IO ()

foreign import ccall safe "nad_app_name"
  c_app_name :: CInt -> IO CString

foreign import ccall safe "nad_secure_input_enabled"
  c_secure_input_enabled :: IO CInt

foreign import ccall safe "nad_secure_input_pid"
  c_secure_input_pid :: IO CInt

foreign import ccall safe "nad_ax_window_title"
  c_ax_window_title :: Ptr () -> IO CString

foreign import ccall safe "nad_ax_window_app"
  c_ax_window_app :: Ptr () -> IO CString

foreign import ccall safe "nad_ax_window_pid"
  c_ax_window_pid :: Ptr () -> IO CInt

foreign import ccall safe "nad_ax_window_frame"
  c_ax_window_frame
    :: Ptr () -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt

foreign import ccall safe "nad_ax_set_window_frame"
  c_ax_set_window_frame :: Ptr () -> CDouble -> CDouble -> CDouble -> CDouble -> IO CInt

foreign import ccall safe "nad_ax_focus_window"
  c_ax_focus_window :: Ptr () -> IO CInt

foreign import ccall unsafe "nad_screen_main_height"
  c_screen_main_height :: IO CDouble

foreign import ccall unsafe "nad_screen_count"
  c_screen_count :: IO CInt

foreign import ccall safe "nad_screen_frame"
  c_screen_frame
    :: CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt

-- | Key code and packed modifiers; non-zero result swallows the key press.
type KeyHandler = Word16 -> Word32 -> IO CInt

foreign import ccall "wrapper"
  mkKeyHandler :: KeyHandler -> IO (FunPtr KeyHandler)

foreign import ccall safe "nad_hotkey_start"
  c_hotkey_start :: FunPtr KeyHandler -> IO CInt

-- | Calls back into Haskell for the lifetime of the process, so it must be a
-- @safe@ import.
foreign import ccall safe "nad_run_loop"
  c_run_loop :: IO ()

foreign import ccall unsafe "nad_stop_run_loop"
  c_stop_run_loop :: IO ()

-- | Matches NAD_SYMBOLIC_HOTKEY_MAX in @cbits/nad.h@.
symbolicHotkeyMax :: Int
symbolicHotkeyMax = 256

foreign import ccall unsafe "nad_symbolic_hotkey_get"
  c_symbolic_hotkey_get :: CInt -> Ptr Word16 -> Ptr Word32 -> IO CInt

foreign import ccall unsafe "nad_symbolic_hotkey_enabled"
  c_symbolic_hotkey_enabled :: CInt -> IO CInt

foreign import ccall unsafe "nad_symbolic_hotkey_set_enabled"
  c_symbolic_hotkey_set_enabled :: CInt -> CInt -> IO ()

-- | Bar calls hop to the main thread inside the shim, so they are @safe@: they
-- block until AppKit has done the work.
foreign import ccall safe "nad_app_init"
  c_app_init :: IO ()

foreign import ccall safe "nad_bar_create"
  c_bar_create
    :: CDouble -> CDouble -> CDouble -> CDouble
    -> CString -> CString -> CString -> CDouble
    -> IO CInt

foreign import ccall safe "nad_bar_set"
  c_bar_set :: CInt -> CString -> CString -> CString -> IO ()

foreign import ccall safe "nad_bar_destroy_all"
  c_bar_destroy_all :: IO ()
