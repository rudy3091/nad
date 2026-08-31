# nad

An xmonad-like tiling window manager for macOS, configured in Haskell.

nad drives the Accessibility API directly, so it works with the Spaces and
full-screen features macOS already has instead of replacing them. No private
APIs, no SIP changes.

## Features

- Tall (master/stack), Full and Stacking layouts, with gaps
- Keyboard focus, window swapping, master ratio and master count
- Nine workspaces, independent of macOS Spaces
- Multi-monitor: each display tiles on its own
- Floating rules for apps that must be left alone
- A configurable status bar drawn as a borderless window per display
- A control socket, so the same actions are available from the shell

## Requirements

GHC 9.10, cabal 3.16, macOS 13 or later.

## Build

```sh
cabal build
cabal test
```

nad needs two permissions, and macOS grants those to an application rather
than to a loose binary. `scripts/bundle.sh` builds `build/nad.app` for that:

```sh
scripts/bundle.sh
```

Then grant, in System Settings › Privacy & Security:

| Permission      | Why                                     |
| --------------- | --------------------------------------- |
| Accessibility   | reading, moving and resizing windows    |
| Input Monitoring | the global hotkeys                     |

Add `build/nad.app` to both lists and run `build/nad.app/Contents/MacOS/nad
doctor` to confirm.

### Keeping the permissions across rebuilds

Unsigned binaries are remembered by path, so every rebuild loses the grant.
Sign with a stable identity instead. Create one in Keychain Access ›
Certificate Assistant › Create a Certificate (name `nad-dev`, type
`Code Signing`, self-signed), then:

```sh
NAD_SIGN_IDENTITY=nad-dev scripts/bundle.sh
```

## Usage

```sh
nad                     # run it
nad query keys          # the active bindings
nad query windows       # what nad can tile
nad query screens       # displays and their usable areas
nad query state         # ask a running nad what it is showing
nad msg focus-next      # same actions as the hotkeys, from the shell
nad msg workspace 3
nad doctor              # permissions, config and socket
```

Default bindings use `cmd-alt`: `j`/`k` to move focus, `shift-j`/`shift-k` to
move a window, `return` to promote to master, `h`/`l` to resize master, `space`
to cycle layouts, `1`–`9` for workspaces, `q` to quit. `nad query keys` prints
the full list.

In the Stacking layout, `cmd-alt-ctrl` plus `h`/`l`/`k`/`j` resizes the focused
window itself (narrower, wider, shorter, taller). The tiled layouts ignore it:
there a window's size comes from its neighbours, so `cmd-alt-h`/`l` on the
master area is the knob to turn.

In Stacking the mouse works too: drag any edge or corner and nad keeps the frame
you dragged it to, as if you had pressed the resize keys. Grabbing the top or the
left side keeps the window where you left it rather than snapping its corner back
to the cascade. Dragging a window's title bar to only move it still hands it back
to the cascade, and the tiled layouts still pull a dragged window back into its
tile, for the same reason they ignore the resize keys.

### System shortcuts

macOS dispatches its own shortcuts — Spotlight, Finder search, Force Quit — in
the WindowServer, ahead of any event tap. A binding that collides with one can
never fire: the system action happens instead.

nad handles this by switching the colliding shortcut off for as long as it runs,
and switching it back on when it exits. On start it reports what it took:

```
nad: took over 1 system shortcut(s) that would have shadowed a binding
```

That is how `cmd-alt-space` cycles layouts (xmonad's key) even though macOS
ships it as "Show Finder search window".

This uses `CGSSetSymbolicHotKeyEnabled`, a private SkyLight function — there is
no public way to do it. It fails soft: if the symbol ever changes, nad loses the
ability to take a shortcut over and nothing else breaks.

The shortcuts are restored on quit, on `ctrl-c` and on `kill`. If nad is killed
outright it cannot restore anything, so the ids it took are written to
`~/.nad/claimed-hotkeys` first and put back on the next start. To undo it by
hand, System Settings › Keyboard › Keyboard Shortcuts has the same switches.

### Secure keyboard entry

An application with secure keyboard entry on — terminals offer it, and password
fields turn it on — makes macOS stop delivering key events to event taps
entirely. Every nad binding is dead for as long as that application is focused,
and no window manager can work around it: that is what the feature is for.

nad warns about it at start-up and `nad doctor` checks for it, because the
symptom otherwise looks like nad being broken in one app and fine everywhere
else.

kitty binds `opt+cmd+s` to this by default and remembers the setting across
restarts, so it is easy to switch on by accident while trying `cmd-alt`
bindings:

```sh
defaults read net.kovidgoyal.kitty SecureKeyboardEntry   # 1 means on
defaults write net.kovidgoyal.kitty SecureKeyboardEntry -bool false
```

To stop it happening again, take the binding away in `kitty.conf`:

```
map opt+cmd+s no_op
```

Terminal.app has the same feature under Edit › Secure Keyboard Entry.

Ordinary application shortcuts need none of this: nad's tap runs first and
swallows the key, so `cmd-alt-h` reaching nad instead of "Hide Others" is
expected.

To see what your machine has taken:

```sh
defaults read com.apple.symbolichotkeys AppleSymbolicHotKeys
nad watch-keys   # or: what does the tap actually see?
```

## Configuration

Like xmonad, the configuration is a Haskell program. Write `~/.nad/nad.hs`:

```haskell
import Nad

main :: IO ()
main =
  nadWith
    defaultConfig
      { cfgWorkspaces = 5
      , cfgFloats = FloatApp "Activity Monitor" : cfgFloats defaultConfig
      , cfgBar =
          defaultBar
            { barPosition = Bottom
            , barRender = \st ->
                BarContent
                  { barLeft = [seg (bsLayout st)]
                  , barCenter = []
                  , barRight = [colored "#88aaff" (bsClock st)]
                  }
            }
      }
```

`nad` compiles it to `~/.nad/nad-<arch>-darwin` and hands over to it, so the
process keeps its permissions. `nad --recompile` rebuilds it; a config that
fails to compile is reported and the previous binary keeps running.

The library has to be visible to GHC for this. Once:

```sh
cabal install --lib nad
```
