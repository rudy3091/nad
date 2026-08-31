// Thin C surface over the macOS APIs nad needs. Every function here is called
// from Nad.Platform.FFI with a 1:1 mapping.
//
// Ownership: functions returning `char *` hand over a malloc'd string the
// caller must free(). Window handles come from nad_ax_list_windows and are
// released by nad_ax_free_windows.
#ifndef NAD_H
#define NAD_H

#include <stdint.h>

// Opaque AXUIElementRef for a window.
typedef void *nad_window;

// Accessibility permission. `prompt` non-zero shows the system dialog.
int nad_ax_trusted(int prompt);

// Allocates *out with `count` retained window handles, or -1 on failure. The
// caller owns both the array (release with nad_free) and each handle (release
// with nad_ax_release).
int nad_ax_list_windows(nad_window **out);
void nad_ax_release(nad_window w);

// Window identity. Two handles can be different pointers for the same window,
// so this is the only correct way to compare them.
int nad_ax_same_window(nad_window a, nad_window b);

// free() for anything the shim malloc'd.
void nad_free(void *p);

char *nad_ax_window_title(nad_window w);
char *nad_ax_window_app(nad_window w);
int nad_ax_window_pid(nad_window w);
// Returns 0 on success. Coordinates are AX global (top-left origin, y down).
int nad_ax_window_frame(nad_window w, double *x, double *y, double *width, double *height);
int nad_ax_set_window_frame(nad_window w, double x, double y, double width, double height);
int nad_ax_focus_window(nad_window w);

// --- NSScreen -------------------------------------------------------------
// Screen geometry is reported in Cocoa coordinates (bottom-left origin, y up).

double nad_screen_main_height(void);
int nad_screen_count(void);
int nad_screen_frame(int index, int visible, double *x, double *y, double *width,
                     double *height);

// --- Hotkeys and the run loop ---------------------------------------------

#define NAD_MOD_CMD 1u
#define NAD_MOD_ALT 2u
#define NAD_MOD_CTRL 4u
#define NAD_MOD_SHIFT 8u

// Called on the main thread for every key press. Return non-zero to swallow the
// event, which is how a bound key is kept from reaching the focused app.
typedef int (*nad_key_handler)(uint16_t keycode, uint32_t modifiers);

// Returns 0 on success, -1 if the tap could not be created (usually a missing
// Input Monitoring permission).
int nad_hotkey_start(nad_key_handler handler);

// Runs the main thread's CFRunLoop. Everything callback-driven — the event tap
// today, window notifications and the bar later — needs this to be running.
void nad_run_loop(void);
void nad_stop_run_loop(void);

// --- Status bar -----------------------------------------------------------
// Bar geometry is Cocoa coordinates. Bars are identified by the index returned
// from nad_bar_create.

// Must be called on the main thread before any bar is created.
void nad_app_init(void);

int nad_bar_create(double x, double y, double width, double height, const char *bg,
                   const char *fg, const char *font_name, double font_size);

// Each argument is a list of "fg\x1fbg\x1ftext" records joined by \x1e. Colours
// are "#RRGGBB", or empty for the bar's default.
void nad_bar_set(int bar, const char *left, const char *center, const char *right);
void nad_bar_destroy_all(void);

#endif
