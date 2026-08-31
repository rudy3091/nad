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
import System.IO (hFlush, hPutStrLn, stderr, stdout)

import Nad.Core.Layout (arrange)
import Nad.Config.Recompile (launchUserConfig, recompile)
import Nad.Paths (compiledPath, configPath, socketPath)
import Nad.Ipc (sendCommand)
import Nad.Runtime (runDaemon)
import Nad.Types.Config (Config (..), bindings, reserveBar, shouldFloat)
import Nad.Platform.Hotkey (runEventLoop, secureInputHolder, startHotkeys)
import Nad.Types.Key (KeyCode (..), KeyCombo (..), Modifier (..), showCombo)
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
  | WatchKeys
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
  ["watch-keys"] -> WatchKeys
  [] -> Daemon
  ["--help"] -> Help
  ["-h"] -> Help
  other -> Unknown other

runCommand :: Config -> Command -> IO ()
runCommand cfg cmd = case cmd of
  -- A user config replaces this process entirely; if there is none, carry on.
  Daemon -> launchUserConfig >> runDaemon cfg
  Recompile -> recompileOnly
  WatchKeys -> watchKeys
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
    , "  nad watch-keys       print every key press nad's tap can see"
    ]

-- | Report what the event tap actually observes.
--
-- This answers the question a dead binding raises: did the key never reach nad,
-- or did nad see it and not have it bound? cmd-alt combinations are swallowed
-- while this runs, so it also shows whether a system shortcut can be taken over
-- at all — if the system action still fires, the tap never had a chance.
watchKeys :: IO ()
watchKeys = do
  started <- startHotkeys $ \combo -> do
    let claimed = Cmd `elem` comboMods combo && Alt `elem` comboMods combo
        KeyCode code = comboKey combo
    putStrLn
      ( pad 24 (showCombo combo)
          <> pad 12 ("keycode " <> show code)
          <> (if claimed then "swallowed" else "passed through")
      )
    hFlush stdout
    pure claimed
  unless started $ do
    hPutStrLn stderr inputMonitoringHelp
    exitFailure
  reportSecureInput
  putStrLn "watching keys. cmd-alt combinations are swallowed; ctrl-c to stop."
  runEventLoop

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
  holder <- secureInputHolder
  putStrLn (check (holder == Nothing) "No application is holding secure keyboard entry")
  forM_ holder (putStrLn . ("\n" <>) . secureInputHelp)
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

pad :: Int -> String -> String
pad n s = s <> replicate (max 1 (n - length s)) ' '

inputMonitoringHelp :: String
inputMonitoringHelp =
  unlines
    [ "nad could not create the event tap, so it cannot see any keys."
    , "Grant Input Monitoring in System Settings › Privacy & Security,"
    , "then run nad again."
    ]

-- | Warn about secure input on the diagnostic paths. Silent when nothing holds
-- it, which is the normal case.
reportSecureInput :: IO ()
reportSecureInput = secureInputHolder >>= mapM_ (hPutStrLn stderr . secureInputHelp)

secureInputHelp :: String -> String
secureInputHelp name =
  unlines
    [ name <> " has secure keyboard entry switched on."
    , "macOS sends no key events to event taps while that is true, so none of"
    , "nad's bindings will fire while it is focused. That is a system guarantee,"
    , "not something nad can work around."
    , ""
    , "Turn it off in that application. In kitty it is opt+cmd+s"
    , "(toggle_macos_secure_keyboard_entry) and the setting is remembered:"
    , "  defaults write net.kovidgoyal.kitty SecureKeyboardEntry -bool false"
    , "In Terminal.app it is the Edit › Secure Keyboard Entry menu item."
    ]

accessibilityHelp :: String
accessibilityHelp =
  unlines
    [ "nad needs the Accessibility permission to see and move windows."
    , "Grant it in System Settings › Privacy & Security › Accessibility,"
    , "then run nad again."
    ]
