// CGEventTap: global hotkeys, plus the run loop everything else hangs off.
#import <Cocoa/Cocoa.h>
#import <CoreGraphics/CoreGraphics.h>

#include "nad.h"

static CFMachPortRef g_tap = NULL;
static nad_key_handler g_handler = NULL;

// Our own modifier encoding, so the Haskell side never sees CG constants.
static uint32_t pack_modifiers(CGEventFlags flags) {
  uint32_t mods = 0;
  if (flags & kCGEventFlagMaskCommand) mods |= NAD_MOD_CMD;
  if (flags & kCGEventFlagMaskAlternate) mods |= NAD_MOD_ALT;
  if (flags & kCGEventFlagMaskControl) mods |= NAD_MOD_CTRL;
  if (flags & kCGEventFlagMaskShift) mods |= NAD_MOD_SHIFT;
  return mods;
}

static CGEventRef on_event(CGEventTapProxy proxy, CGEventType type, CGEventRef event,
                           void *refcon) {
  (void)proxy;
  (void)refcon;

  // The system disables a tap that takes too long, or after a wake. Neither is
  // an error we can do anything about except switch it back on.
  if (type == kCGEventTapDisabledByTimeout || type == kCGEventTapDisabledByUserInput) {
    if (g_tap != NULL) CGEventTapEnable(g_tap, true);
    return event;
  }

  if (type != kCGEventKeyDown || g_handler == NULL) return event;

  uint16_t keycode = (uint16_t)CGEventGetIntegerValueField(event, kCGKeyboardEventKeycode);
  uint32_t mods = pack_modifiers(CGEventGetFlags(event));

  // Non-zero means the key was bound, so the app underneath must not see it.
  if (g_handler(keycode, mods)) return NULL;
  return event;
}

int nad_hotkey_start(nad_key_handler handler) {
  if (g_tap != NULL) return 0;
  g_handler = handler;

  g_tap = CGEventTapCreate(kCGSessionEventTap, kCGHeadInsertEventTap,
                           kCGEventTapOptionDefault, CGEventMaskBit(kCGEventKeyDown),
                           on_event, NULL);
  // The only realistic cause is a missing Input Monitoring permission.
  if (g_tap == NULL) {
    g_handler = NULL;
    return -1;
  }

  CFRunLoopSourceRef source = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, g_tap, 0);
  CFRunLoopAddSource(CFRunLoopGetMain(), source, kCFRunLoopCommonModes);
  CFRelease(source);
  CGEventTapEnable(g_tap, true);
  return 0;
}

void nad_run_loop(void) { CFRunLoopRun(); }

void nad_stop_run_loop(void) { CFRunLoopStop(CFRunLoopGetMain()); }
