// The status bar: one borderless window per screen.
//
// Bars are referred to by index into a static array rather than by pointer,
// because handing an ARC-managed object across the FFI would mean bridging
// retains by hand at every call.
#import <Cocoa/Cocoa.h>

#include <string.h>

#include "nad.h"

@interface NadBar : NSObject
@property(strong) NSWindow *window;
@property(strong) NSTextField *left;
@property(strong) NSTextField *center;
@property(strong) NSTextField *right;
@property(strong) NSFont *font;
@property(strong) NSColor *foreground;
@end

@implementation NadBar
@end

static NSMutableArray<NadBar *> *g_bars = nil;

// AppKit is not thread safe, and nad's worker is not the main thread. Calls that
// come from the main thread already (never, today) must not deadlock on
// dispatch_sync, hence the check.
static void on_main(dispatch_block_t block) {
  if ([NSThread isMainThread]) {
    block();
  } else {
    dispatch_sync(dispatch_get_main_queue(), block);
  }
}

void nad_app_init(void) {
  on_main(^{
    // Accessory: nad has windows but no Dock icon and never takes focus.
    [NSApplication sharedApplication];
    [NSApp setActivationPolicy:NSApplicationActivationPolicyAccessory];
  });
}

// "#RRGGBB", or NULL/empty for the fallback.
static NSColor *parse_color(const char *spec, NSColor *fallback) {
  if (spec == NULL || strlen(spec) != 7 || spec[0] != '#') return fallback;
  unsigned int value = 0;
  if (sscanf(spec + 1, "%x", &value) != 1) return fallback;
  return [NSColor colorWithSRGBRed:((value >> 16) & 0xFF) / 255.0
                             green:((value >> 8) & 0xFF) / 255.0
                              blue:(value & 0xFF) / 255.0
                             alpha:1.0];
}

static NSTextField *make_label(NSTextAlignment alignment, NSFont *font, NSColor *color) {
  NSTextField *field = [[NSTextField alloc] initWithFrame:NSZeroRect];
  field.bezeled = NO;
  field.editable = NO;
  field.selectable = NO;
  field.drawsBackground = NO;
  field.alignment = alignment;
  field.font = font;
  field.textColor = color;
  return field;
}

int nad_bar_create(double x, double y, double width, double height, const char *bg,
                   const char *fg, const char *font_name, double font_size) {
  __block int result = -1;
  on_main(^{
    if (g_bars == nil) g_bars = [NSMutableArray array];

    NSRect frame = NSMakeRect(x, y, width, height);
    NSWindow *window = [[NSWindow alloc] initWithContentRect:frame
                                                  styleMask:NSWindowStyleMaskBorderless
                                                    backing:NSBackingStoreBuffered
                                                      defer:NO];
    window.backgroundColor = parse_color(bg, [NSColor blackColor]);
    window.opaque = NO;
    window.hasShadow = NO;
    // Above ordinary windows, and present on every Space so the bar does not
    // vanish when the user switches with macOS's own gesture.
    window.level = NSStatusWindowLevel;
    window.collectionBehavior =
        NSWindowCollectionBehaviorCanJoinAllSpaces | NSWindowCollectionBehaviorStationary;
    // The bar is a readout, not a control: clicks belong to whatever is under it.
    window.ignoresMouseEvents = YES;

    NSFont *font = [NSFont fontWithName:[NSString stringWithUTF8String:font_name ?: ""]
                                   size:font_size];
    if (font == nil) font = [NSFont monospacedSystemFontOfSize:font_size weight:NSFontWeightRegular];
    NSColor *color = parse_color(fg, [NSColor whiteColor]);

    NadBar *bar = [[NadBar alloc] init];
    bar.window = window;
    bar.font = font;
    bar.foreground = color;
    bar.left = make_label(NSTextAlignmentLeft, font, color);
    bar.center = make_label(NSTextAlignmentCenter, font, color);
    bar.right = make_label(NSTextAlignmentRight, font, color);

    // Thirds. Each label is vertically centred in the bar.
    CGFloat third = width / 3.0;
    CGFloat textHeight = font.pointSize + 4;
    CGFloat top = (height - textHeight) / 2.0;
    CGFloat pad = 8;
    bar.left.frame = NSMakeRect(pad, top, third - pad, textHeight);
    bar.center.frame = NSMakeRect(third, top, third, textHeight);
    bar.right.frame = NSMakeRect(2 * third, top, third - pad, textHeight);

    NSView *content = window.contentView;
    [content addSubview:bar.left];
    [content addSubview:bar.center];
    [content addSubview:bar.right];

    [window orderFrontRegardless];

    [g_bars addObject:bar];
    result = (int)g_bars.count - 1;
  });
  return result;
}

// Segments arrive as "fg\x1fbg\x1ftext" records joined by \x1e. Empty colour
// fields mean "use the bar's default".
static NSAttributedString *parse_segments(const char *encoded, NSFont *font,
                                          NSColor *defaultColor) {
  NSMutableAttributedString *out = [[NSMutableAttributedString alloc] init];
  if (encoded == NULL) return out;

  NSString *input = [NSString stringWithUTF8String:encoded];
  for (NSString *record in [input componentsSeparatedByString:@"\x1e"]) {
    if (record.length == 0) continue;
    NSArray<NSString *> *fields = [record componentsSeparatedByString:@"\x1f"];
    if (fields.count != 3) continue;

    NSMutableDictionary *attrs = [NSMutableDictionary dictionary];
    attrs[NSFontAttributeName] = font;
    attrs[NSForegroundColorAttributeName] =
        parse_color(fields[0].UTF8String, defaultColor);
    NSColor *background = parse_color(fields[1].UTF8String, nil);
    if (background != nil) attrs[NSBackgroundColorAttributeName] = background;

    [out appendAttributedString:[[NSAttributedString alloc] initWithString:fields[2]
                                                               attributes:attrs]];
  }
  return out;
}

void nad_bar_set(int index, const char *left, const char *center, const char *right) {
  on_main(^{
    if (g_bars == nil || index < 0 || index >= (int)g_bars.count) return;
    NadBar *bar = g_bars[index];
    bar.left.attributedStringValue = parse_segments(left, bar.font, bar.foreground);
    bar.center.attributedStringValue = parse_segments(center, bar.font, bar.foreground);
    bar.right.attributedStringValue = parse_segments(right, bar.font, bar.foreground);
  });
}

void nad_bar_destroy_all(void) {
  on_main(^{
    for (NadBar *bar in g_bars) [bar.window orderOut:nil];
    [g_bars removeAllObjects];
  });
}
