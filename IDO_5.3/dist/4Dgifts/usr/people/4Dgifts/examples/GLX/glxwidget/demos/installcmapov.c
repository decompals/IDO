#include <X11/Intrinsic.h>
#include <X11/Xirisw/GlxMDraw.h>
/*
 * This routine is similar to installColormap (in the file installcmap.c)
 * excepth that it weill also install any auxiliary (overlay, popup and
 * underlay) windows onto the top level window.  It may not be called
 * until after the windows have been realized.
 */
installColormapWithOverlay(toplevel, glw)
Widget toplevel, glw;
{
    Window windows[5];
    Window overlay, popup, underlay;
    Arg args[5];
    register int i=0;

    i=0;
    XtSetArg(args[i], GlxNoverlayWindow, &overlay); i++;
    XtSetArg(args[i], GlxNpopupWindow, &popup); i++;
    XtSetArg(args[i], GlxNunderlayWindow, &underlay); i++;
    XtGetValues(glw, args, i);
    i = 0;
    if (overlay)
    {
	windows[i] = overlay;
	i++;
    }
    if (popup)
    {
	windows[i] = popup;
	i++;
    }
    if (underlay)
    {
	windows[i] = underlay;
	i++;
    }
    windows[i] = XtWindow(glw); i++;
    windows[i] = XtWindow(toplevel); i++;
    XSetWMColormapWindows(XtDisplay(toplevel), XtWindow(toplevel), windows, i);
}
