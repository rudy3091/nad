# Revision history for nad

## Unreleased

* Multi-monitor rework. Every display now shows a workspace of its own, as in
  xmonad, instead of all of them switching together. A window's display follows
  from its workspace rather than from its coordinates, which fixes windows piling
  onto the first display after a workspace round trip.
* `cmd-alt-3` shows a workspace on the focused display, swapping with whichever
  display already holds it.
* New `FocusScreen` action, bound to `cmd-alt-left` / `cmd-alt-right`, moving the
  keyboard between displays.
* `cmd-alt-shift-left` / `cmd-alt-shift-right` now send the focused window to the
  neighbouring display's workspace rather than repositioning it by hand.
* Each display's status bar marks its own workspace. `BarState` gains
  `bsOtherScreens`, the workspaces showing on the other displays.
* New `cfgBorder`: an outline around the focused window, 5px soft white by
  default.
* `cfgPin` now decides the display a window *opens* on rather than holding it
  there for good.
* `nad query state` reports the display-to-workspace bindings.
* New `nad restart`: quits a running nad through the control socket, waits for
  it to exit, and starts in its place. Quitting rather than killing means the
  system shortcuts it took over are handed back.

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
