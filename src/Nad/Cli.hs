-- | Argument parsing. The same binary is the daemon, the control client and
-- the diagnostic tool depending on how it is invoked.
module Nad.Cli
  ( Command (..)
  , parseCommand
  , runCommand
  , usage
  ) where

import Control.Monad (forM_, unless)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Nad.Core.Layout (arrange)
import Nad.Config.Recompile (compiledPath, configPath, launchUserConfig, recompile)
import Nad.Ipc (sendCommand, socketPath)
import Nad.Runtime (runDaemon)
import Nad.Types.Config (Config (..), bindings, reserveBar, shouldFloat)
import Nad.Types.Key (showCombo)
import Nad.Core.Action (showAction)
import Nad.Platform.Screen (listScreens)
import Nad.Platform.Window (isTrusted, listWindows, requestTrust, setWindowFrame)
import Nad.Types.Window

data Command
  = Daemon
  | QueryWindows
  | QueryScreens
  | QueryKeys
  | QueryState
  | Message [String]
  | Recompile
  | Tile
  | Doctor
  | Help
  | Unknown [String]
  deriving (Eq, Show)

parseCommand :: [String] -> Command
parseCommand args = case args of
  ["query", "windows"] -> QueryWindows
  ["query", "screens"] -> QueryScreens
  ["query", "keys"] -> QueryKeys
  ["query", "state"] -> QueryState
  ("msg" : rest) | not (null rest) -> Message rest
  ["tile"] -> Tile
  ["doctor"] -> Doctor
  ["--recompile"] -> Recompile
  [] -> Daemon
  ["--help"] -> Help
  ["-h"] -> Help
  other -> Unknown other

runCommand :: Config -> Command -> IO ()
runCommand cfg cmd = case cmd of
  -- A user config replaces this process entirely; if there is none, carry on.
  Daemon -> launchUserConfig >> runDaemon cfg
  Recompile -> recompileOnly
  QueryKeys -> mapM_ putStrLn (describeKeys cfg)
  QueryState -> ask ["state"]
  Message args -> ask args
  QueryWindows -> withTrust (listWindows >>= mapM_ (putStrLn . describeWindow))
  QueryScreens -> listScreens >>= mapM_ (putStrLn . describeScreen)
  Tile -> withTrust (tileOnce cfg)
  Doctor -> doctor
  Help -> putStr usage
  Unknown args -> do
    hPutStrLn stderr ("nad: unknown command: " <> unwords args)
    hPutStrLn stderr usage
    exitFailure

usage :: String
usage =
  unlines
    [ "nad — a tiling window manager for macOS"
    , ""
    , "usage:"
    , "  nad                  run the window manager"
    , "  nad msg <action>     tell a running nad to do something"
    , "  nad query keys       list the active key bindings"
    , "  nad query state      ask a running nad what it is showing"
    , "  nad query windows    list the windows nad can tile"
    , "  nad query screens    list displays and their usable areas"
    , "  nad tile             apply the default layout once, then exit"
    , "  nad --recompile      rebuild ~/.nad/nad.hs"
    , "  nad doctor           check permissions and setup"
    ]

recompileOnly :: IO ()
recompileOnly = do
  source <- configPath
  result <- recompile True
  case result of
    Left err -> hPutStrLn stderr err >> exitFailure
    Right Nothing -> putStrLn ("nad: no config at " <> source <> ", using defaults")
    Right (Just binary) -> putStrLn ("nad: built " <> binary)

-- | Talk to a running daemon. Its reply is passed through verbatim.
ask :: [String] -> IO ()
ask args = do
  path <- socketPath
  reply <- sendCommand path args
  case reply of
    Left err -> hPutStrLn stderr err >> exitFailure
    Right out -> putStr out

-- | Run an action that needs the Accessibility permission, prompting for it.
withTrust :: IO () -> IO ()
withTrust action = do
  trusted <- requestTrust
  unless trusted $ do
    hPutStrLn stderr accessibilityHelp
    exitFailure
  action

-- | One-shot tiling: no state, no daemon. This is the check that the layout
-- maths and the AX setters agree with what the screen does.
tileOnce :: Config -> IO ()
tileOnce cfg = case cfgLayouts cfg of
  [] -> pure ()
  spec : _ -> do
    screens <- map (reserveBar (cfgBar cfg)) <$> listScreens
    windows <- filter (not . shouldFloat (cfgFloats cfg)) <$> listWindows
    forM_ screens $ \screen -> do
      let onThisScreen w =
            fmap screenIndex (screenFor screens (winFrame w)) == Just (screenIndex screen)
      forM_ (arrange spec (screenUsable screen) (filter onThisScreen windows)) $
        \(w, rect) -> setWindowFrame (winRef w) rect

doctor :: IO ()
doctor = do
  trusted <- isTrusted
  putStrLn (check trusted "Accessibility permission")
  screens <- listScreens
  putStrLn (check (not (null screens)) ("Display enumeration (" <> show (length screens) <> " screens)"))
  count <- if trusted then length <$> listWindows else pure 0
  putStrLn (check (count > 0) ("Window enumeration (" <> show count <> " windows)"))
  source <- configPath
  hasConfig <- doesFileExist source
  binary <- compiledPath
  putStrLn ("  info " <> if hasConfig then "Config " <> source <> " -> " <> binary else "No config; using built-in defaults")
  path <- socketPath
  running <- sendCommand path ["state"]
  putStrLn (check (either (const False) (const True) running) "Daemon reachable on the control socket")
  unless trusted (putStrLn ("\n" <> accessibilityHelp))
  where
    check ok label = (if ok then "  ok   " else "  FAIL ") <> label

-- | Bindings as they are actually understood, so a typo in a key name is
-- visible rather than a key that quietly does nothing.
describeKeys :: Config -> [String]
describeKeys cfg =
  [pad 24 (showCombo combo) <> showAction action | (combo, action) <- keymap]
    <> ["unparseable: " <> name | name <- bad]
  where
    (keymap, bad) = bindings cfg
    pad n s = s <> replicate (max 1 (n - length s)) ' '

accessibilityHelp :: String
accessibilityHelp =
  unlines
    [ "nad needs the Accessibility permission to see and move windows."
    , "Grant it in System Settings › Privacy & Security › Accessibility,"
    , "then run nad again."
    ]
