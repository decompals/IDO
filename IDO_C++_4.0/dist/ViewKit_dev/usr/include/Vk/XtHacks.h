#include <X11/Intrinsic.h>

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif
extern void ForceShellIconic(Widget w);
extern char * XtClassName(Widget w);
extern int isPoppedUp(Widget w);
extern int isBeingDestroyed(Widget w);
extern void setPopupCursors(Widget, const Cursor);
#if defined(__cplusplus) || defined(c_plusplus)
}
#endif
