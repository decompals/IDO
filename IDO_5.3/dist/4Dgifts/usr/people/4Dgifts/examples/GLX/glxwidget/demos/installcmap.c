#include <X11/Intrinsic.h>

/*
 * This routine will install a particular gl widgets's colormap onto the
 * top level window.  It may not be called until after the windows have
 * been realized.
 */
installColormap(toplevel, glw)
Widget toplevel, glw;
{
    Window windows[2];

    windows[0] = XtWindow(glw);
    windows[1] = XtWindow(toplevel);
    XSetWMColormapWindows(XtDisplay(toplevel), XtWindow(toplevel), windows, 2);
}
