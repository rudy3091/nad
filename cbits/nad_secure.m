// Secure keyboard entry: is it on, and who turned it on.
//
// While an application has it on, the WindowServer delivers no key events to
// event taps at all — that is the point of it. nad's hotkeys stop working for as
// long as that application is focused and there is no way around it from user
// space. All nad can do is say so, clearly.
#import <Foundation/Foundation.h>
#import <Carbon/Carbon.h>
#import <IOKit/IOKitLib.h>

#include "nad.h"

int nad_secure_input_enabled(void) { return IsSecureEventInputEnabled() ? 1 : 0; }

// Best effort only: the console-user record carries a pid, but the WindowServer
// fills it in for the active application. A holder that is not frontmost — or is
// not an application at all — leaves it empty, so callers must cope with 0 while
// nad_secure_input_enabled still reports 1.
int nad_secure_input_pid(void) {
  io_registry_entry_t entry =
      IORegistryEntryFromPath(kIOMainPortDefault, "IOService:/IOResources/IOConsoleUsers");
  if (entry == MACH_PORT_NULL) return 0;

  CFTypeRef property =
      IORegistryEntryCreateCFProperty(entry, CFSTR("IOConsoleUsers"), kCFAllocatorDefault, 0);
  IOObjectRelease(entry);
  if (property == NULL) return 0;

  int pid = 0;
  if (CFGetTypeID(property) == CFArrayGetTypeID()) {
    for (NSDictionary *session in (__bridge NSArray *)property) {
      NSNumber *secure = session[@"kCGSSessionSecureInputPID"];
      if (secure != nil && secure.intValue != 0) {
        pid = secure.intValue;
        break;
      }
    }
  }
  CFRelease(property);
  return pid;
}
