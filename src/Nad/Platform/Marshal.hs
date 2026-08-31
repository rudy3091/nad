-- | Marshalling helpers shared by the "Nad.Platform" modules.
module Nad.Platform.Marshal
  ( withRectOut
  , peekOwnedCString
  ) where

import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CDouble (..), CInt)
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)

import Nad.Types.Geometry (Rect (..))

-- | Call a shim function that fills four out-parameters with x, y, width,
-- height and returns 0 on success.
withRectOut
  :: (Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt)
  -> IO (Maybe Rect)
withRectOut call =
  alloca $ \px -> alloca $ \py -> alloca $ \pw -> alloca $ \ph -> do
    status <- call px py pw ph
    if status /= 0
      then pure Nothing
      else
        Just
          <$> ( Rect
                  <$> peekDouble px
                  <*> peekDouble py
                  <*> peekDouble pw
                  <*> peekDouble ph
              )
  where
    peekDouble p = (\(CDouble d) -> d) <$> peek p

-- | The shim hands over malloc'd strings; copy, then release.
peekOwnedCString :: CString -> IO String
peekOwnedCString p
  | p == nullPtr = pure ""
  | otherwise = do
      s <- peekCString p
      free p
      pure s
