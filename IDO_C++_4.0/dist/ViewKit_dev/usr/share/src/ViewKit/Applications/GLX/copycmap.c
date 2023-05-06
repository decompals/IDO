/*
 * This file includes the function CopyGlColormap that creates a new
 * colormap for a GlxMDraw widget.  The colors specified
 * by the colorInfo structure (describe in copycmap.h) are allocated
 * in the colormap
 * When possible, they will match the pixel value in parent,
 * to avoid having incorrect colors installed in the X-based portion of
 * the application.
 * This function cannot be called until the widget is realized.
 */

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xirisw/GlxMDraw.h>
#include <X11/Xm/Xm.h>
#include "copycmap.h"

CopyGlColormap(glw, colorInfo, colorInfoSize)
Widget glw;
struct glxcColorInfo *colorInfo;
int colorInfoSize;
{
    XVisualInfo *visualInfo;
    int depth;
    int maxcolor;
    Colormap colormap;
    Colormap pmap;
    Arg args[10];
    int n;
    register i;
    XColor rgb_db_def;
    Display *display = XtDisplay(glw);

    /* first get the visual and depth*/
    n = 0;
    XtSetArg(args[n], XtNvisual, &visualInfo); n++;
    XtSetArg(args[n], XtNdepth, &depth); n++;
    XtGetValues(glw, args, n);
    maxcolor = 1<<depth;
    /* Create a new colormap */
    colormap = XCreateColormap(display, XtWindow(glw),
			       visualInfo->visual, AllocAll);
    /* Add the colormap to the window */
    n = 0;
    XtSetArg(args[n], XtNcolormap, colormap); n++;
    XtSetValues(glw, args, n);

    /* get the parent's colormap */
    n = 0;
    XtSetArg(args[n], XtNcolormap, &pmap); n++;
    XtGetValues(XtParent(glw), args, n);

    /* for each color in the colorInfo, determine the correct pixel value */
    for (i=0; i<colorInfoSize; i++)
    {
	colorInfo[i].color.flags = DoRed|DoGreen|DoBlue;
	switch (colorInfo[i].type)
	{
	case GLXC_ABSOLUTE:
	    colorInfo[i].color.pixel = (Pixel)colorInfo[i].value;
	    XQueryColor (display, pmap, &colorInfo[i].color);
	    break;
	case GLXC_NAMED:
	    if (!XAllocNamedColor(display, pmap, (char *)colorInfo[i].value,
				  &colorInfo[i].color, &rgb_db_def))
	    {
		fprintf (stderr, "could not allocate %s\n", (char *)colorInfo[i].value);
		exit (1);
	    }
	    break;
	case GLXC_RESOURCE:
	    n = 0;
	    XtSetArg(args[n], colorInfo[i].value, &colorInfo[i].color.pixel); n++;
	    XtGetValues(glw, args, n);
	    XQueryColor (display, pmap, &colorInfo[i].color);
	    break;
	default:
	    fprintf (stderr, "unknown type in colorInfo\n");
	    exit (1);
	}
    }
    /* We have determined all the colors.  Loop through the colors and
     * make sure that they fit into the GL colormap.  If they do not,
     * choose another color that fits into the colormap.
     * After this check, store the colors into the colormap,
     * and save in the variables */
    for (i=0; i<colorInfoSize; i++)
    {
	if (colorInfo[i].color.pixel >= maxcolor)
	{
	    /* it was too high.  find another pixel to use.
	     * start at the highest color and work down
	     */
	    register try, check;

	    for (try = maxcolor-1; try >= 0 ; try--)
	    {
		for (check = 0; check < colorInfoSize; check++)
		{
		    if (colorInfo[check].color.pixel == try)
			break;
		}
		if (check == colorInfoSize) /* didn't find a match */
		{
		    colorInfo[i].color.pixel = try;
		    break;
		}
	    }
	}
	XStoreColor(display, colormap, &colorInfo[i].color);
	*colorInfo[i].result = (Colorindex)colorInfo[i].color.pixel;
    }
}	
