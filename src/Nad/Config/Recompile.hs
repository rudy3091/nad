-- | The xmonad trick: a user's configuration is a Haskell program that links
-- against this library, and nad replaces itself with it.
--
-- @~\/.nad\/nad.hs@, if present, is compiled to @~\/.nad\/nad-\<arch\>@ and
-- exec'd in place of the shipped binary. Since the shipped binary is what the
-- user launched, this is transparent: the process keeps its pid and its
-- permissions.
module Nad.Config.Recompile
  ( configPath
  , compiledPath
  , recompile
  , launchUserConfig
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import System.Directory
  ( doesFileExist
  , getHomeDirectory
  , getModificationTime
  )
import System.Environment (getExecutablePath, getArgs)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Info (arch)
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (executeFile)
import System.Process (readProcessWithExitCode)

configPath :: IO FilePath
configPath = (\home -> home </> ".nad" </> "nad.hs") <$> getHomeDirectory

compiledPath :: IO FilePath
compiledPath = (\home -> home </> ".nad" </> ("nad-" <> arch <> "-darwin")) <$> getHomeDirectory

-- | Build @nad.hs@ if it exists and is newer than its binary.
--
-- Returns the compiler's complaints on failure. A failed build deliberately
-- leaves the previous binary in place: a typo in a config should not cost the
-- user their window manager.
recompile :: Bool -> IO (Either String (Maybe FilePath))
recompile force = do
  source <- configPath
  present <- doesFileExist source
  if not present
    then pure (Right Nothing)
    else do
      binary <- compiledPath
      stale <- if force then pure True else isStale source binary
      if not stale
        then pure (Right (Just binary))
        else do
          home <- getHomeDirectory
          (code, _, err) <-
            readProcessWithExitCode
              "ghc"
              [ "-v0"
              , "-o", binary
              , "-outputdir", home </> ".nad" </> "build"
              , "-package", "nad"
              , source
              ]
              ""
          pure $ case code of
            ExitSuccess -> Right (Just binary)
            ExitFailure _ -> Left err

isStale :: FilePath -> FilePath -> IO Bool
isStale source binary = do
  built <- doesFileExist binary
  if not built
    then pure True
    else do
      sourceTime <- getModificationTime source
      binaryTime <- getModificationTime binary
      pure (sourceTime > binaryTime)

-- | Hand over to the user's configuration, if there is one. Returns only when
-- there is nothing to hand over to, so the caller can carry on with the
-- built-in configuration.
launchUserConfig :: IO ()
launchUserConfig = do
  self <- getExecutablePath
  built <- recompile False
  case built of
    Left err -> hPutStrLn stderr ("nad: config failed to compile, using defaults\n" <> err)
    Right Nothing -> pure ()
    Right (Just binary) -> do
      -- Guard against a user config that forgets to call `nad` and re-execs the
      -- shipped binary, which would loop forever.
      unless (binary == self) $ do
        args <- getArgs
        result <- try (executeFile binary False args Nothing) :: IO (Either SomeException ())
        either (\e -> hPutStrLn stderr ("nad: could not start config: " <> show e)) pure result
