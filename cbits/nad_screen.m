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

int nad_screen_count(void) { return (int)[NSScreen screens].count; }

// `visible` non-zero asks for visibleFrame, which excludes the menu bar and the
// Dock — that is the area windows may actually use.
int nad_screen_frame(int index, int visible, double *x, double *y, double *width,
                     double *height) {
  NSArray<NSScreen *> *screens = [NSScreen screens];
  if (index < 0 || index >= (int)screens.count) return -1;

  NSRect frame = visible ? screens[index].visibleFrame : screens[index].frame;
  *x = frame.origin.x;
  *y = frame.origin.y;
  *width = frame.size.width;
  *height = frame.size.height;
  return 0;
}
