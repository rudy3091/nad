// Accessibility API: enumerate and inspect windows.
#import <Cocoa/Cocoa.h>
#import <ApplicationServices/ApplicationServices.h>
#include <stdlib.h>
#include <string.h>

#include "nad.h"

// An unresponsive app would otherwise block the whole WM on a single AX call.
static const float kMessagingTimeout = 1.0f;

static char *copy_utf8(NSString *s) {
  if (s == nil) return strdup("");
  return strdup([s UTF8String]);
}

// Returns a +1 retained attribute value, or NULL.
static CFTypeRef copy_attr(AXUIElementRef element, CFStringRef attr) {
  CFTypeRef value = NULL;
  if (AXUIElementCopyAttributeValue(element, attr, &value) != kAXErrorSuccess) return NULL;
  return value;
}

// A "standard window" is what a user thinks of as a window: no sheets, popovers
// or tooltips. Those must stay where the app put them.
static BOOL is_standard_window(AXUIElementRef window) {
  CFTypeRef subrole = copy_attr(window, kAXSubroleAttribute);
  if (subrole == NULL) return NO;
  BOOL standard = CFGetTypeID(subrole) == CFStringGetTypeID() &&
                  CFStringCompare(subrole, kAXStandardWindowSubrole, 0) == kCFCompareEqualTo;
  CFRelease(subrole);
  return standard;
}

int nad_ax_trusted(int prompt) {
  NSDictionary *options = @{(__bridge NSString *)kAXTrustedCheckOptionPrompt : @(prompt != 0)};
  return AXIsProcessTrustedWithOptions((__bridge CFDictionaryRef)options) ? 1 : 0;
}

int nad_ax_list_windows(nad_window **out) {
  if (out == NULL) return -1;
  *out = NULL;
  if (!AXIsProcessTrustedWithOptions(NULL)) return -1;

  NSMutableArray *collected = [NSMutableArray array];

  for (NSRunningApplication *app in [[NSWorkspace sharedWorkspace] runningApplications]) {
    // Agents and background-only processes own no user-facing windows.
    if (app.activationPolicy != NSApplicationActivationPolicyRegular) continue;

    AXUIElementRef appElement = AXUIElementCreateApplication(app.processIdentifier);
    if (appElement == NULL) continue;
    AXUIElementSetMessagingTimeout(appElement, kMessagingTimeout);

    CFTypeRef windows = copy_attr(appElement, kAXWindowsAttribute);
    if (windows != NULL) {
      if (CFGetTypeID(windows) == CFArrayGetTypeID()) {
        CFIndex count = CFArrayGetCount(windows);
        for (CFIndex i = 0; i < count; i++) {
          AXUIElementRef window = (AXUIElementRef)CFArrayGetValueAtIndex(windows, i);
          if (window == NULL || !is_standard_window(window)) continue;
          [collected addObject:(__bridge id)window];
        }
      }
      CFRelease(windows);
    }
    CFRelease(appElement);
  }

  int count = (int)collected.count;
  if (count == 0) return 0;

  nad_window *handles = calloc((size_t)count, sizeof(nad_window));
  if (handles == NULL) return -1;
  for (int i = 0; i < count; i++) {
    handles[i] = (nad_window)CFRetain((CFTypeRef)collected[i]);
  }
  *out = handles;
  return count;
}

void nad_ax_release(nad_window w) {
  if (w != NULL) CFRelease((CFTypeRef)w);
}

int nad_ax_same_window(nad_window a, nad_window b) {
  if (a == NULL || b == NULL) return a == b;
  return CFEqual((CFTypeRef)a, (CFTypeRef)b) ? 1 : 0;
}

void nad_free(void *p) { free(p); }

char *nad_ax_window_title(nad_window w) {
  CFTypeRef title = copy_attr((AXUIElementRef)w, kAXTitleAttribute);
  if (title == NULL || CFGetTypeID(title) != CFStringGetTypeID()) {
    if (title != NULL) CFRelease(title);
    return strdup("");
  }
  char *result = copy_utf8((__bridge NSString *)title);
  CFRelease(title);
  return result;
}

int nad_ax_window_pid(nad_window w) {
  pid_t pid = 0;
  if (AXUIElementGetPid((AXUIElementRef)w, &pid) != kAXErrorSuccess) return -1;
  return (int)pid;
}

char *nad_app_name(int pid) {
  if (pid <= 0) return strdup("");
  NSRunningApplication *app = [NSRunningApplication runningApplicationWithProcessIdentifier:pid];
  return copy_utf8(app.localizedName);
}

char *nad_ax_window_app(nad_window w) { return nad_app_name(nad_ax_window_pid(w)); }

int nad_ax_window_frame(nad_window w, double *x, double *y, double *width, double *height) {
  CFTypeRef posValue = copy_attr((AXUIElementRef)w, kAXPositionAttribute);
  CFTypeRef sizeValue = copy_attr((AXUIElementRef)w, kAXSizeAttribute);
  int status = -1;

  CGPoint origin;
  CGSize size;
  if (posValue != NULL && sizeValue != NULL &&
      AXValueGetValue(posValue, kAXValueCGPointType, &origin) &&
      AXValueGetValue(sizeValue, kAXValueCGSizeType, &size)) {
    *x = origin.x;
    *y = origin.y;
    *width = size.width;
    *height = size.height;
    status = 0;
  }

  if (posValue != NULL) CFRelease(posValue);
  if (sizeValue != NULL) CFRelease(sizeValue);
  return status;
}

static int set_point(AXUIElementRef window, CFStringRef attr, CGPoint point) {
  AXValueRef value = AXValueCreate(kAXValueCGPointType, &point);
  if (value == NULL) return -1;
  AXError err = AXUIElementSetAttributeValue(window, attr, value);
  CFRelease(value);
  return err == kAXErrorSuccess ? 0 : -1;
}

static int set_size(AXUIElementRef window, CFStringRef attr, CGSize size) {
  AXValueRef value = AXValueCreate(kAXValueCGSizeType, &size);
  if (value == NULL) return -1;
  AXError err = AXUIElementSetAttributeValue(window, attr, value);
  CFRelease(value);
  return err == kAXErrorSuccess ? 0 : -1;
}

int nad_ax_set_window_frame(nad_window w, double x, double y, double width, double height) {
  AXUIElementRef window = (AXUIElementRef)w;
  CGPoint origin = CGPointMake(x, y);
  CGSize size = CGSizeMake(width, height);

  // Order matters. A window still at its old position may refuse a size that
  // would push it off its current screen, and a window still at its old size
  // may have its position clamped. Size, move, then size again: the second
  // resize lands once the window is somewhere the size is legal.
  set_size(window, kAXSizeAttribute, size);
  int moved = set_point(window, kAXPositionAttribute, origin);
  int resized = set_size(window, kAXSizeAttribute, size);
  return (moved == 0 && resized == 0) ? 0 : -1;
}

int nad_ax_focus_window(nad_window w) {
  AXUIElementRef window = (AXUIElementRef)w;
  // Raising the window is not enough; the owning app has to come forward too,
  // or keyboard input keeps going to whatever was active before.
  if (AXUIElementSetAttributeValue(window, kAXMainAttribute, kCFBooleanTrue) != kAXErrorSuccess)
    return -1;
  AXUIElementPerformAction(window, kAXRaiseAction);

  pid_t pid = 0;
  if (AXUIElementGetPid(window, &pid) != kAXErrorSuccess) return -1;
  NSRunningApplication *app = [NSRunningApplication runningApplicationWithProcessIdentifier:pid];
  // Options 0, not ignoringOtherApps: that flag is a no-op since macOS 14.
  [app activateWithOptions:0];
  return 0;
}
