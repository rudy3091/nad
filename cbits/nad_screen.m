// NSScreen: display geometry.
//
// Everything here reports Cocoa coordinates (bottom-left origin, y up).
// Nad.Platform.Screen flips them into AX coordinates using nad_screen_main_height.
#import <Cocoa/Cocoa.h>

#include "nad.h"

double nad_screen_main_height(void) {
  NSArray<NSScreen *> *screens = [NSScreen screens];
  if (screens.count == 0) return 0.0;
  // NSScreen.screens[0] is the display holding the menu bar, which is the
  // origin of the global coordinate space both APIs are expressed in.
  return screens[0].frame.size.height;
}

// Screens nad may tile. While vertical-ipad-screenshare runs, its "Vertical
// iPad" virtual display is what the user sees on the iPad; the landscape
// Sidecar display only carries pixels and is hidden under a mirror window, so
// windows tiled there would be invisible.
static NSArray<NSScreen *> *tiled_screens(void) {
  NSArray<NSScreen *> *screens = [NSScreen screens];
  NSPredicate *vertical = [NSPredicate predicateWithFormat:@"localizedName == 'Vertical iPad'"];
  if ([screens filteredArrayUsingPredicate:vertical].count == 0) return screens;
  return [screens filteredArrayUsingPredicate:
                      [NSPredicate predicateWithFormat:@"NOT (localizedName CONTAINS 'Sidecar')"]];
}

int nad_screen_count(void) { return (int)tiled_screens().count; }

// `visible` non-zero asks for visibleFrame, which excludes the menu bar and the
// Dock — that is the area windows may actually use.
int nad_screen_frame(int index, int visible, double *x, double *y, double *width,
                     double *height) {
  NSArray<NSScreen *> *screens = tiled_screens();
  if (index < 0 || index >= (int)screens.count) return -1;

  NSRect frame = visible ? screens[index].visibleFrame : screens[index].frame;
  *x = frame.origin.x;
  *y = frame.origin.y;
  *width = frame.size.width;
  *height = frame.size.height;
  return 0;
}
