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
import Control.Monad (forM, forM_, forever, unless, void, when)
import Data.IORef
import Data.List (find)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Posix.Signals (Handler (..), installHandler, sigINT, sigTERM)

import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)

import Nad.Bar.Segment (BarState (..))
import Nad.Core.Action (Action (..), parseAction)
import Nad.Core.Layout (arrangeWith, draggedPlacements, layoutName)
import Nad.Core.Stack (Stack (..))
import Nad.Core.State
import Nad.Ipc (serve, socketPath)
import Nad.Platform.Bar (Bar (..), createBars, initApp, updateBar)
import Nad.Platform.Border (hideBorder, showBorder)
import Nad.Platform.Hotkey
  ( claimSystemHotkeys
  , releaseStaleHotkeys
  , releaseSystemHotkeys
  , runEventLoop
  , secureInputHolder
  , startHotkeys
  , stopEventLoop
  )
import Nad.Platform.Screen (listScreens, mainScreenHeight)
import Nad.Platform.Window (focusWindow, listWindows, requestTrust, setWindowFrame)
import Nad.Types.Config
  ( BarConfig (..)
  , BorderConfig
  , Config (..)
  , bindings
  , reserveBar
  , ruleValue
  , shouldFloat
  )
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

  -- A previous run that was killed outright may have left system shortcuts
  -- switched off. Put them back before deciding what to claim this time.
  releaseStaleHotkeys
  claimed <- claimSystemHotkeys (map fst keymap)
  unless (null claimed) $
    hPutStrLn stderr
      ( "nad: took over "
          <> show (length claimed)
          <> " system shortcut(s) that would have shadowed a binding"
      )

  -- Every exit path has to give them back, including ctrl-c and `kill`.
  forM_ [sigINT, sigTERM] $ \signal ->
    installHandler signal (Catch (releaseSystemHotkeys claimed >> stopEventLoop)) Nothing

  tapped <- startHotkeys $ \combo ->
    case lookup combo keymap of
      Nothing -> pure False
      Just action -> do
        atomically (writeTQueue queue (Perform action))
        pure True
  unless tapped $ do
    hPutStrLn stderr "nad: could not create the event tap. Grant Input Monitoring, then retry."
    exitFailure

  -- Worth saying once at start: with secure input on, every binding is dead
  -- while that application is focused, and nothing in nad's logs would show it.
  holder <- secureInputHolder
  forM_ holder $ \name ->
    hPutStrLn stderr
      ( "nad: warning — "
          <> name
          <> " has secure keyboard entry on; bindings will not fire while it is"
          <> " focused. See `nad doctor`."
      )

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
  releaseSystemHotkeys claimed

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
      : ("screens    " <> unwords (map showing (stVisible st)))
      : ("layout     " <> layoutName (currentLayout st))
      : ("windows    " <> show (length (stackItems (currentStack st))))
      : [ "focus      " <> maybe "none" (const "set") (focusedWindow st) ]
  where
    marked ws =
      let label = show (wsId ws) <> ":" <> show (length (stackItems (wsStack ws)))
       in if wsId ws `elem` visibleWorkspaces st then "[" <> label <> "]" else label

    -- The focused display is the one keys apply to, so mark it.
    showing (s, n) =
      show s <> "=" <> show n <> (if s == stScreen st then "*" else "")

worker :: Config -> [Bar] -> TQueue Event -> IORef (WMState WindowRef) -> IO ()
worker cfg bars queue state = loop
  where
    loop = do
      event <- atomically (readTQueue queue)
      windows <- listWindows
      -- Layouts tile what is left after the bar has taken its strip.
      screens <- map (reserveBar (cfgBar cfg)) <$> listScreens

      let tileable = filter (not . shouldFloat (cfgFloats cfg)) windows
      st0 <- readIORef state
      -- Displays first: which workspace a window belongs to can depend on
      -- which display is showing what, and a display that has just been
      -- plugged in needs a workspace before anything is laid out on it.
      let st1 = syncScreens (map screenIndex screens) st0
          st2 = syncWindows (opensOn cfg tileable st1) (map winRef tileable) st1
          -- Before anything else, because the layout pass at the end runs on
          -- every event while the poll only comes once a second: a hotkey
          -- pressed in between would otherwise re-tile over a drag nad had not
          -- recorded yet, and put the window back for good.
          dragged = adoptDrags screens tileable st2
          st3 = case event of
            Refresh -> dragged
            Perform action -> apply action dragged
      writeIORef state st3

      focusFrame <- reconcile screens tileable st3
      when (event /= Refresh) (refocus tileable st3)
      paintBars (cfgBar cfg) bars tileable st3
      paintBorder (cfgBorder cfg) focusFrame

      if stRunning st3 then loop else hideBorder >> stopEventLoop

-- | Where a window nad has never seen before opens: the workspace a rule names,
-- or the one showing on the display a rule pins it to. Rules match on app name
-- and title, which only 'WindowInfo' carries, so this resolves a 'WindowRef'
-- back to one first.
opensOn :: Config -> [WindowInfo] -> WMState WindowRef -> WindowRef -> Maybe Int
opensOn cfg windows st ref = do
  w <- find ((== ref) . winRef) windows
  case ruleValue (cfgAssign cfg) w of
    Just n -> Just n
    Nothing -> ruleValue (cfgPin cfg) w >>= (`workspaceOn` st)

-- | Redraw every bar from the state the worker just settled on.
paintBars :: BarConfig -> [Bar] -> [WindowInfo] -> WMState WindowRef -> IO ()
paintBars _ [] _ _ = pure ()
paintBars cfg bars windows st = do
  clock <- formatTime defaultTimeLocale "%H:%M" <$> getZonedTime
  -- A bar whose display has been unplugged has nothing to say. Its window is
  -- still around until nad restarts; leave the last thing it drew on it.
  forM_ [bar | bar <- bars, barScreen bar `elem` map fst (stVisible st)] $ \bar ->
    updateBar bar (barRender cfg (barState clock (barScreen bar) windows st))

-- | What one bar says. Every display shows its own workspace, so this is
-- computed per bar rather than once for all of them.
barState :: String -> Int -> [WindowInfo] -> WMState WindowRef -> BarState
barState clock screen windows st =
  BarState
    { bsWorkspaces =
        [ (wsId ws, Just (wsId ws) == here, length (stackItems (wsStack ws)))
        | ws <- stWorkspaces st
        ]
    , bsOtherScreens = [n | n <- visibleWorkspaces st, Just n /= here]
    , bsLayout = layoutName (currentLayout st)
    , bsFocused = maybe "" winTitle focused
    , bsScreen = screen
    , bsClock = clock
    }
  where
    here = workspaceOn screen st
    focused =
      stackFocus (stackOn screen st) >>= \ref -> find ((== ref) . winRef) windows

-- | Make the screen match the state, and report the frame the focused window
-- ended up with so the border can be drawn on it.
--
-- Every display lays out the workspace it is showing; windows of a workspace no
-- display is showing are parked off screen.
reconcile
  :: [ScreenInfo] -> [WindowInfo] -> WMState WindowRef -> IO (Maybe Rect)
reconcile screens windows st = do
  placed <- fmap concat $ forM (onScreens screens windows st) $ \(screen, here) -> do
    let sizing w = placeOf st (winRef w)
        frames = arrangeWith (currentLayout st) (screenUsable screen) sizing here
    forM_ frames $ \(w, rect) -> void (setWindowFrame (winRef w) rect)
    pure frames
  forM_ hidden $ \w -> void (setWindowFrame (winRef w) (parkingSpot screens (winFrame w)))
  pure $ do
    ref <- focusedWindow st
    lookup ref [(winRef w, rect) | (w, rect) <- placed]
  where
    shown = concatMap (stackItems . (`stackOn` st) . screenIndex) screens
    hidden = [w | w <- windows, winRef w `notElem` shown]

-- | What each display shows, in stack order. This is the split both the layout
-- pass and drag adoption work from.
--
-- A window's display follows from the workspace it is on, so a window keeps its
-- display across a workspace switch even though it spent the meantime parked
-- off screen at coordinates that are on no display at all.
onScreens :: [ScreenInfo] -> [WindowInfo] -> WMState WindowRef -> [(ScreenInfo, [WindowInfo])]
onScreens screens windows st =
  [ (screen, ordered (stackItems (stackOn (screenIndex screen) st)))
  | screen <- screens
  ]
  where
    -- Follow the stack's order, not the order macOS happened to report.
    ordered refs = [w | ref <- refs, Just w <- [find ((== ref) . winRef) windows]]

-- | Let the mouse resize a window in the Stacking layout: instead of putting it
-- back where the layout wanted it on the next poll, keep the frame the user
-- dragged it to. Its origin comes along, or grabbing the top-right corner would
-- pull the window up and to the left while the pointer is still on it.
adoptDrags :: [ScreenInfo] -> [WindowInfo] -> WMState WindowRef -> WMState WindowRef
adoptDrags screens windows st = foldl' adopt st drags
  where
    adopt s (w, placement) = place (winRef w) placement s

    spec = currentLayout st
    sizing w = placeOf st (winRef w)

    drags =
      concat
        [ draggedPlacements spec area (Just . winFrame) (arrangeWith spec area sizing here)
        | (screen, here) <- onScreens screens windows st
        , let area = screenUsable screen
        ]

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

-- | Outline the focused window, or hide the outline when nothing is focused.
--
-- The frame comes from the layout pass rather than from the poll, so the
-- outline never lags a window that has just been re-tiled. Floating windows are
-- not in the layout at all and so never get one.
paintBorder :: BorderConfig -> Maybe Rect -> IO ()
paintBorder cfg frame = case frame of
  Nothing -> hideBorder
  Just r -> do
    mainHeight <- mainScreenHeight
    showBorder cfg mainHeight r
