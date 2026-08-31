-- | Control socket. Both halves live here because they are two ends of the
-- same three-line protocol: the client writes one line of words, the server
-- writes back a reply and closes.
--
-- The words are the same ones 'Nad.Core.Action.parseAction' understands, so
-- @nad msg focus-next@ and the @cmd-alt-j@ binding reach the runtime through
-- the same path.
module Nad.Ipc
  ( socketPath
  , serve
  , sendCommand
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forever, void)
import qualified Data.ByteString.Char8 as BS
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (doesFileExist, removeFile)

import Nad.Paths (socketPath)

-- | Accept connections forever on a background thread.
--
-- The handler runs on that thread, so it must not block on the runtime queue
-- for long — it is expected to enqueue and return.
serve :: FilePath -> ([String] -> IO String) -> IO ()
serve path handler = do
  -- A socket file left behind by a crash would otherwise make bind fail, and
  -- nothing is listening on it by definition.
  stale <- doesFileExist path
  if stale then removeFile path else pure ()

  sock <- socket AF_UNIX Stream defaultProtocol
  bind sock (SockAddrUnix path)
  listen sock 8
  void . forkIO . forever $ do
    (conn, _) <- accept sock
    void . forkIO $ do
      result <- try (handleOne conn) :: IO (Either SomeException ())
      either (const (pure ())) pure result
      close conn
  where
    handleOne conn = do
      request <- recv conn 4096
      reply <- handler (words (BS.unpack request))
      sendAll conn (BS.pack reply)

-- | Send one command to a running daemon. 'Left' means no daemon answered.
sendCommand :: FilePath -> [String] -> IO (Either String String)
sendCommand path args = do
  result <- try attempt :: IO (Either SomeException String)
  pure $ case result of
    Left _ -> Left "nad: no daemon is listening. Start it with `nad`."
    Right reply -> Right reply
  where
    attempt =
      bracket (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
        connect sock (SockAddrUnix path)
        sendAll sock (BS.pack (unwords args))
        BS.unpack <$> readAll sock

    readAll sock = do
      chunk <- recv sock 4096
      if BS.null chunk then pure BS.empty else (chunk <>) <$> readAll sock
