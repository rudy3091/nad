-- | The daemon: one queue, one worker.
--
-- Every source of work — the event tap, the poll timer, and later the control
-- socket — pushes an 'Event' onto a single queue. The worker is the only thing
-- that touches state or moves windows, so there is no locking and no ordering
-- question. The main thread does nothing but run CFRunLoop, which is what the
-- event tap and the bar require.
module Nad.Runtime
  ( Event (..)
  , runDaemon
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forM_, forever, unless, void, when)
import Data.IORef
import Data.List (find)
import Data.Maybe (listToMaybe)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)

import Nad.Bar.Segment (BarState (..))
import Nad.Core.Action (Action (..), Direction (..), parseAction)
import Nad.Core.Layout (arrange, layoutName)
import Nad.Core.Stack (Stack (..))
import Nad.Core.State
import Nad.Ipc (serve, socketPath)
import Nad.Platform.Bar (Bar (..), createBars, initApp, updateBar)
import Nad.Platform.Hotkey (runEventLoop, startHotkeys, stopEventLoop)
import Nad.Platform.Screen (listScreens, mainScreenHeight)
import Nad.Platform.Window (focusWindow, listWindows, requestTrust, setWindowFrame)
import Nad.Types.Config (BarConfig (..), Config (..), bindings, reserveBar, shouldFloat)
import Nad.Types.Geometry (Rect (..), rectRight)
import Nad.Types.Window

data Event
  = Perform Action
  | -- | Something may have changed on screen: re-read windows and re-tile.
    Refresh
  deriving (Eq, Show)

-- | How often to notice windows that opened or closed.
--
-- ponytail: a poll, because AXObserver needs per-app observers with their own
-- lifecycle. One second is below the threshold where it feels broken; swap in
-- notifications if it ever feels slow.
pollInterval :: Int
pollInterval = 1000000

runDaemon :: Config -> IO ()
runDaemon cfg = do
  trusted <- requestTrust
  unless trusted $ do
    hPutStrLn stderr "nad: the Accessibility permission is required. Run `nad doctor`."
    exitFailure

  let (keymap, unparseable) = bindings cfg
  forM_ unparseable $ \name ->
    hPutStrLn stderr ("nad: ignoring unparseable key binding: " <> name)

  queue <- newTQueueIO
  state <- newIORef (initialState (cfgWorkspaces cfg) (cfgLayouts cfg))

  tapped <- startHotkeys $ \combo ->
    case lookup combo keymap of
      Nothing -> pure False
      Just action -> do
        atomically (writeTQueue queue (Perform action))
        pure True
  unless tapped $ do
    hPutStrLn stderr "nad: could not create the event tap. Grant Input Monitoring, then retry."
    exitFailure

  void $ forkIO $ forever $ do
    threadDelay pollInterval
    atomically (writeTQueue queue Refresh)

  path <- socketPath
  serve path (control queue state)

  -- AppKit has to be woken up on the main thread before any window exists.
  initApp
  bars <-
    if barEnabled (cfgBar cfg)
      then do
        mainHeight <- mainScreenHeight
        createBars (cfgBar cfg) mainHeight =<< listScreens
      else pure []

  void $ forkIO (worker cfg bars queue state)

  atomically (writeTQueue queue Refresh)
  runEventLoop

-- | Answer a control-socket request. Actions are queued so the worker stays the
-- only thing that touches state; queries read the last state it published.
control :: TQueue Event -> IORef (WMState WindowRef) -> [String] -> IO String
control queue state args = case args of
  ["state"] -> describeState <$> readIORef state
  _ -> case parseAction args of
    Just action -> do
      atomically (writeTQueue queue (Perform action))
      pure "ok\n"
    Nothing -> pure ("nad: not a command: " <> unwords args <> "\n")

describeState :: WMState WindowRef -> String
describeState st =
  unlines $
    ("workspace  " <> unwords (map marked (stWorkspaces st)))
      : ("layout     " <> layoutName (currentLayout st))
      : ("windows    " <> show (length (stackItems (currentStack st))))
      : [ "focus      " <> maybe "none" (const "set") (focusedWindow st) ]
  where
    marked ws =
      let label = show (wsId ws) <> ":" <> show (length (stackItems (wsStack ws)))
       in if wsId ws == stCurrent st then "[" <> label <> "]" else label

worker :: Config -> [Bar] -> TQueue Event -> IORef (WMState WindowRef) -> IO ()
worker cfg bars queue state = loop
  where
    loop = do
      event <- atomically (readTQueue queue)
      windows <- listWindows
      -- Layouts tile what is left after the bar has taken its strip.
      screens <- map (reserveBar (cfgBar cfg)) <$> listScreens

      let tileable0 = filter (not . shouldFloat (cfgFloats cfg)) windows
      st0 <- readIORef state
      let st1 = syncWindows (map winRef tileable0) st0
          st2 = case event of
            Refresh -> st1
            Perform action -> apply action st1
      writeIORef state st2

      -- Moving between screens is the one action that has to touch a window
      -- before the layout pass, because which screen a window belongs to is
      -- read back off its position.
      tileable <- case event of
        Perform (MoveToScreen dir) -> sendToScreen screens tileable0 st2 dir
        _ -> pure tileable0

      reconcile screens tileable st2
      when (event /= Refresh) (refocus tileable st2)
      paintBars (cfgBar cfg) bars tileable st2

      if stRunning st2 then loop else stopEventLoop

-- | Redraw every bar from the state the worker just settled on.
paintBars :: BarConfig -> [Bar] -> [WindowInfo] -> WMState WindowRef -> IO ()
paintBars _ [] _ _ = pure ()
paintBars cfg bars windows st = do
  clock <- formatTime defaultTimeLocale "%H:%M" <$> getZonedTime
  forM_ bars $ \bar ->
    updateBar bar (barRender cfg (barState clock (barScreen bar) windows st))

barState :: String -> Int -> [WindowInfo] -> WMState WindowRef -> BarState
barState clock screen windows st =
  BarState
    { bsWorkspaces =
        [ (wsId ws, wsId ws == stCurrent st, length (stackItems (wsStack ws)))
        | ws <- stWorkspaces st
        ]
    , bsLayout = layoutName (currentLayout st)
    , bsFocused = maybe "" winTitle focused
    , bsScreen = screen
    , bsClock = clock
    }
  where
    focused = focusedWindow st >>= \ref -> find ((== ref) . winRef) windows

-- | Make the screen match the state.
--
-- Windows of the current workspace are placed by the layout of the screen they
-- sit on; windows of every other workspace are parked off screen.
reconcile :: [ScreenInfo] -> [WindowInfo] -> WMState WindowRef -> IO ()
reconcile screens windows st = do
  forM_ screens $ \screen -> do
    let here = [w | w <- visible, assignedTo w == Just (screenIndex screen)]
    forM_ (arrange (currentLayout st) (screenUsable screen) here) $ \(w, rect) ->
      void (setWindowFrame (winRef w) rect)
  forM_ hidden $ \w -> void (setWindowFrame (winRef w) (parkingSpot screens (winFrame w)))
  where
    order = stackItems (currentStack st)
    -- Follow the stack's order, not the order macOS happened to report.
    visible = [w | ref <- order, Just w <- [find ((== ref) . winRef) windows]]
    hidden = [w | w <- windows, winRef w `notElem` order]

    -- A window returning from another workspace is still parked off screen, so
    -- it belongs to no display. Give it the first one rather than dropping it.
    assignedTo w = case screenFor screens (winFrame w) of
      Just s -> Just (screenIndex s)
      Nothing -> screenIndex <$> listToMaybe screens

-- | Where hidden windows wait: just past the right edge of every display, at
-- the height they already had so the window keeps its shape.
--
-- ponytail: an app that repositions its own window will pull it back into view,
-- and a full-screen app cannot be parked at all. The alternative is the private
-- Spaces API, which breaks on every macOS release.
parkingSpot :: [ScreenInfo] -> Rect -> Rect
parkingSpot screens r = r {rectX = edge + 100, rectY = 0}
  where
    edge = maximum (0 : map (rectRight . screenFrame) screens)

-- | Tell macOS about the focus nad believes in. Only after a user action: doing
-- it on every poll would fight the user clicking on windows.
refocus :: [WindowInfo] -> WMState WindowRef -> IO ()
refocus windows st = case focusedWindow st of
  Nothing -> pure ()
  Just ref -> forM_ (find ((== ref) . winRef) windows) (void . focusWindow . winRef)

-- | Drop the focused window onto the next or previous screen and report the
-- window list with its new position, so the layout pass that follows assigns it
-- to the screen it just landed on.
sendToScreen
  :: [ScreenInfo] -> [WindowInfo] -> WMState WindowRef -> Direction -> IO [WindowInfo]
sendToScreen screens windows st dir
  | length screens < 2 = pure windows
  | otherwise = case focusedWindow st >>= \ref -> find ((== ref) . winRef) windows of
      Nothing -> pure windows
      Just w -> case targetScreen w of
        Nothing -> pure windows
        Just target -> do
          void (setWindowFrame (winRef w) (screenUsable target))
          pure [if winRef x == winRef w then x {winFrame = screenUsable target} else x | x <- windows]
  where
    step = case dir of
      Next -> 1
      Prev -> -1

    targetScreen w = do
      current <- screenFor screens (winFrame w)
      i <- lookup (screenIndex current) (zip (map screenIndex screens) [0 ..])
      pure (screens !! ((i + step) `mod` length screens))
