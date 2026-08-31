-- | Where nad keeps its things. One place, so @~\/.nad@ is spelled once.
module Nad.Paths
  ( nadDir
  , socketPath
  , configPath
  , compiledPath
  , claimedHotkeysPath
  ) where

import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))
import System.Info (arch)

-- | @~\/.nad@, created if missing.
nadDir :: IO FilePath
nadDir = do
  home <- getHomeDirectory
  let dir = home </> ".nad"
  createDirectoryIfMissing True dir
  pure dir

socketPath :: IO FilePath
socketPath = (</> "nad.sock") <$> nadDir

configPath :: IO FilePath
configPath = (</> "nad.hs") <$> nadDir

compiledPath :: IO FilePath
compiledPath = (</> ("nad-" <> arch <> "-darwin")) <$> nadDir

-- | System hotkeys nad has switched off.
--
-- Written before they are disabled, so that a nad killed outright — no chance
-- to restore anything — still leaves a record the next start can act on. A
-- user whose Spotlight quietly stopped working would have no way to guess why.
claimedHotkeysPath :: IO FilePath
claimedHotkeysPath = (</> "claimed-hotkeys") <$> nadDir
