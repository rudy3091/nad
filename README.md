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
