#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

/* This routine converts a widget's background pixel to an RGB color
 * suitable for gl.
 */

unsigned long
WidgetBackgroundToGlRgb(widget)
Widget widget;
{
    Arg args[10];
    int n;
    Pixel xbg;		/* x background pixel */
    Colormap xcolormap;
    XColor xcolor;
    unsigned long glbg;	/* gl bacground color */

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

    /* Now pack into an RGB color suitable for GL.  The format is 0x00BBGGRR
     * Since the x format has colors values from 0 to 65535 first shift the
     * x values right by 8 and then shift left to fit into the rgb color.*/
    glbg = (xcolor.red >> 8) + ((xcolor.green >> 8) << 8) +
	((xcolor.blue >> 8) << 16);
    return (glbg);
}
	

    
