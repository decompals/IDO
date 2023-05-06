#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

/* This routine converts a widget's background pixel to an RGB color
 * suitable for gl in the c3i format.
 */

unsigned long
WidgetBackgroundToGlC3i(widget,cv)
Widget widget;
long cv[3];
{
    Arg args[10];
    int n;
    Pixel xbg;		/* x background pixel */
    Colormap xcolormap;
    XColor xcolor;

    /* First get the background pixel from the widget. */
    n = 0;
    XtSetArg(args[n], XtNbackground, &xbg); n++;
    XtGetValues(widget, args, n);

    /* Now get the colormap from the top level.  We can't use the widget's
     * colormap because it might not contain the same colors as the
     * colors that the background color were allocated from, so we use it's
     * parent
     */
    n = 0;
    XtSetArg(args[n], XtNcolormap, &xcolormap); n++;
    XtGetValues(XtParent(widget), args, n);

    /* Now obtain RGB values */
    xcolor.flags = DoRed | DoGreen | DoBlue;
    xcolor.pixel = xbg;
    XQueryColor (XtDisplay(widget), xcolormap, &xcolor);

    /* Now store into the color vector.
     * Since the x format has colors values from 0 to 65535 first shift the
     * x values right by 8 and then shift left to fit into the 8 bit GL colors.
     */
    cv[0] = xcolor.red >> 8;
    cv[1] = xcolor.green >> 8;
    cv[2] = xcolor.blue >> 8;
}
	

    
