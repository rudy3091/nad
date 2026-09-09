---
name: nad-config
description: >
  Write or edit a nad window manager configuration (~/.nad/nad.hs): key
  bindings, which workspace or display an app opens on, layouts and gaps, the
  status bar. Use when the user asks to configure nad, rebind a nad key, pin an
  app to a workspace, or when their nad config fails to compile.
---

# Writing a nad configuration

nad's config is a Haskell program at `~/.nad/nad.hs`, like xmonad's. It imports
`Nad`, adjusts `defaultConfig` and passes it to `nadWith`. nad compiles it to
`~/.nad/nad-<arch>-darwin` and execs it in place.

## Workflow

1. **Read `~/.nad/nad.hs` first** if it exists. The user may have edited it by
   hand; never overwrite what is already there, edit it.
2. **Change only the fields that need changing.** Record update syntax on
   `defaultConfig` — do not spell out a whole `Config`, and do not restate a
   field at its default value.
3. **Verify by compiling: `nad --recompile`.** This is not optional. A config
   that fails to compile is *not* an error the user will notice — nad prints
   the failure to stderr and keeps running the previously built binary. Read
   the GHC error, fix it, and run it again until it prints `nad: built ...`.
4. **Check the result** with `nad query keys` (bindings that parsed, and any
   that did not) or `nad query state` (a running nad's workspaces).
5. A binding change reaches a running nad only after `nad restart`. Say so, or
   run it.

If `nad --recompile` fails with `Could not find module 'Nad'`, the library is
not visible to GHC. That is a one-time setup step in the nad checkout:

```sh
cabal install --lib nad
```

## Smallest config that works

```haskell
import Nad

main :: IO ()
main = nadWith defaultConfig {cfgWorkspaces = 5}
```

`example.hs` next to this file is a fuller one that exercises every setting;
copy from it rather than inventing syntax. It is kept compiling, so it is
trustworthy — inside a nad checkout, `cabal exec -- ghc -fno-code
.claude/skills/nad-config/example.hs` proves it without installing anything.

## Config

`import Nad` brings in everything below — one import is always enough.

| Field           | Type                | Default           | What it does                                     |
| --------------- | ------------------- | ----------------- | ------------------------------------------------ |
| `cfgKeys`       | `[(String, Action)]`| 20 + 18 bindings  | Bindings, as `("cmd-alt-j", Focus Next)`         |
| `cfgLayouts`    | `[LayoutSpec]`      | `defaultLayouts`  | Head is active; `CycleLayout` rotates the list   |
| `cfgFloats`     | `[Rule]`            | 4 rules           | Windows left exactly where their app put them    |
| `cfgAssign`     | `[(Rule, Int)]`     | `[]`              | The workspace a window opens on                  |
| `cfgPin`        | `[(Rule, Int)]`     | `[]`              | The display a window opens on                    |
| `cfgWorkspaces` | `Int`               | `9`               | How many workspaces exist                        |
| `cfgBar`        | `BarConfig`         | `defaultBar`      | The status bar                                   |
| `cfgBorder`     | `BorderConfig`      | `defaultBorder`   | The outline around the focused window            |

## Workspaces and displays

Each display shows one workspace, and no two displays ever show the same one —
the xmonad model. This is what decides which display a window is on: a window
belongs to a workspace, and the workspace is on a display. Coordinates never
come into it, so a window keeps its display across a workspace switch.

- `View 3` (`cmd-alt-3`) shows workspace 3 **on the focused display only**. If
  another display is already showing it, the two displays trade workspaces.
- `FocusScreen Next` (`cmd-alt-right`) moves the keyboard to the next display.
  Everything else — `Focus`, `Swap`, `View`, `MoveToWorkspace` — applies to
  whichever display has it.
- `MoveToScreen Next` (`cmd-alt-shift-right`) sends the focused window to the
  workspace the next display is showing. The keyboard stays behind.
- Plugging a display in gives it the lowest workspace nothing else is showing;
  unplugging one leaves its windows on their workspace, reachable with `View`.
  The status bar still needs a nad restart to appear on a new display.

nad cannot see a window the user focused with the mouse, so clicking on the
other display does not move the keyboard there. `cmd-alt-right` does.

## Rules

```haskell
data Rule = RuleApp String    -- application name, matched exactly
          | RuleTitle String  -- window title, matched as a substring
```

All three rule settings match the same way, and the first matching rule wins.

```haskell
  { cfgFloats = RuleApp "Activity Monitor" : cfgFloats defaultConfig
  , cfgAssign = [(RuleApp "kitty", 1), (RuleApp "Safari", 2)]
  , cfgPin    = [(RuleApp "kitty", 0)]
  }
```

- `cfgAssign` names a workspace, `1` to `cfgWorkspaces`. It applies **the first
  time nad sees a window and never again**, so a window moved by hand with
  `cmd-alt-shift-N` stays where the user put it. A number outside the range is
  ignored. Windows already open when the rule is added are not moved.
- `cfgPin` names a display index, the one `nad query screens` prints, where `0`
  is the display with the menu bar. The window opens on whatever workspace that
  display is showing at the time, so like `cfgAssign` it applies **once** and
  the user can move the window afterwards. `cfgAssign` wins when both match, and
  a rule naming a display that is not attached is ignored.

## Layouts

```haskell
data Layout = Tall | Full | Stacking

data LayoutSpec = LayoutSpec
  { specLayout      :: Layout
  , specMasterRatio :: Double  -- master column's share of the width, 0.1–0.9
  , specMasterCount :: Int     -- windows in the master column, at least 1
  , specGap         :: Double  -- points between tiles and around the edge
  }

defaultLayouts =
  [ LayoutSpec Tall     0.55 1 8
  , LayoutSpec Full     0.55 1 0
  , LayoutSpec Stacking 0.55 1 8
  ]
```

- `Tall` — master column on the left, the rest stacked on the right.
- `Full` — every window fills the area; only the raised one shows.
- `Stacking` — a cascade. The only layout that honours mouse drags and the
  `ResizeWindow` keys; the tiled layouts take a window's size from its
  neighbours, so they ignore both.

Only listing `Tall` means `CycleLayout` has nothing to cycle to. That is a
legitimate way to switch the feature off.

## Actions

```haskell
Focus Next | Focus Prev            -- move focus along the window order
Swap Next  | Swap Prev             -- move the focused window along it
SwapMaster                         -- promote the focused window to master
ResizeMaster Double                -- grow/shrink the master area, e.g. 0.05
IncMaster Int                      -- windows sharing the master area, e.g. 1
ResizeWindow Double Double         -- dw dh, fractions of the screen; Stacking only
CycleLayout
View Int                           -- show workspace N on the focused display
MoveToWorkspace Int                -- send the focused window to workspace N
MoveToScreen Next | Prev           -- send it to the next display's workspace
FocusScreen Next  | Prev           -- move the keyboard to the next display
Retile                             -- re-apply the layout
Quit
```

The same actions are reachable from the shell as `nad msg focus-next`,
`nad msg workspace 3`, `nad msg move-to-workspace 3`,
`nad msg focus-screen-next`, and so on.

## Key bindings

Modifiers first, in any order, `-` separated, then exactly one key. **All
lowercase.**

- Modifiers: `cmd` `alt` `ctrl` `shift` — and nothing else. `command`, `opt`,
  `super`, `meta`, `Cmd` are all rejected.
- Keys: `a`–`z`, `0`–`9`, `return` `tab` `space` `delete` `escape` `left`
  `right` `down` `up` `comma` `period` `slash` `minus` `equal` `grave` — and
  nothing else. There are no function keys, no `pageup`, no bracket keys.

```haskell
  { cfgKeys = cfgKeys defaultConfig <> [("cmd-alt-t", Retile)] }
```

A name that does not parse is reported at start-up and by `nad query keys`, not
silently dropped — but it still leaves the user with a dead key, so get it
right.

nad's own defaults all use `cmd-alt`. Keep new bindings on the same modifier
unless the user asks otherwise: `cmd` alone collides with nearly every app.

## Status bar

```haskell
data BarConfig = BarConfig
  { barEnabled    :: Bool     -- True
  , barPosition   :: BarPosition  -- Top | Bottom
  , barHeight     :: Double   -- 26
  , barFont       :: String   -- "SF Mono"; falls back to the system monospaced font
  , barFontSize   :: Double   -- 13
  , barBackground :: String   -- "#1b1b1b"
  , barForeground :: String   -- "#dddddd"
  , barRender     :: BarState -> BarContent  -- defaultRender
  }
```

`barRender` is a pure function, not a format string — that is the whole point
of it. It cannot do IO, so a segment showing the battery or the weather is not
possible; everything it can say comes from `BarState`.

```haskell
data BarState = BarState
  { bsWorkspaces   :: [(Int, Bool, Int)]  -- id, showing on *this* bar's display, window count
  , bsOtherScreens :: [Int]               -- workspaces showing on the other displays
  , bsLayout       :: String
  , bsFocused      :: String              -- focused window title here, "" when none
  , bsScreen       :: Int                 -- which display this bar is on
  , bsClock        :: String              -- "HH:MM"
  }

data BarContent = BarContent {barLeft, barCenter, barRight :: [Segment]}
data Segment = Segment {segText :: String, segFg, segBg :: Maybe String}

seg          :: String -> Segment            -- plain text
colored      :: String -> String -> Segment  -- colour first, then text
emptyContent :: BarContent
defaultRender :: BarState -> BarContent
```

Colours are `"#RRGGBB"` strings.

Each display's bar gets its own `BarState`, so `bsWorkspaces` marks a different
workspace on each. `bsOtherScreens` is what lets a renderer show "on the other
display" as a third state, distinct from both current and hidden.

## Focus border

```haskell
data BorderConfig = BorderConfig
  { borderEnabled :: Bool    -- True
  , borderWidth   :: Double  -- 5, points, drawn just outside the window's edge
  , borderColor   :: String  -- "#cccccc", a soft white
  }
```

An overlay nad draws around the focused window — with more than one display it
is the only thing saying which display the keyboard is on. Floating windows
(`cfgFloats`) are not tracked by nad's focus, so they never get one.

```haskell
  { cfgBorder = defaultBorder {borderColor = "#ff8800"} }
  { cfgBorder = defaultBorder {borderEnabled = False} }
```

## Traps

- **`defaultKeys` and `defaultFloats` are not exported.** To extend a default,
  read it back through `defaultConfig`: `cfgKeys defaultConfig <> [...]`,
  `RuleApp "X" : cfgFloats defaultConfig`. Naming `defaultKeys` directly is a
  scope error.
- **`colored` takes the colour first, the text second.** Both are `String`, so
  swapping them compiles and produces nonsense on screen.
- Do not `import Nad.Types.Key`. Keys are configured as plain strings.
- `nad --recompile` always rebuilds. Plain `nad` and `nad restart` only rebuild
  when `nad.hs` is newer than its binary — so after the nad *library* is
  reinstalled, a bare restart re-runs the same stale binary and the change
  appears not to have taken. Use `nad --recompile && nad restart` there.
- A config that forgets to call `nadWith` (or `nad`) does nothing at all — nad
  execs it and it exits.
- If bindings do nothing in one app only, that is secure keyboard entry, not
  the config: `nad doctor` explains it. If a binding collides with a macOS
  shortcut, nad takes that shortcut over while it runs; the README's
  "System shortcuts" section has the detail.

## When the nad source is at hand

Inside a nad checkout, the last word on all of the above is the code:
`src/Nad/Types/Config.hs` for `Config` and the rules, `src/Nad/Core/Action.hs`
for the action list, `src/Nad/Types/Key.hs` for the key table, and the export
list in `src/Nad.hs` for what a config may name. Check there before guessing.
