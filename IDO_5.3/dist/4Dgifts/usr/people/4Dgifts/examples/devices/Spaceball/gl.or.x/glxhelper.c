#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <gl/glws.h>
#include "glxhelper.h"

/*
 * This file provides a helper function "GLXCreateWindow", which does
 * all the necessary magic to create an X window suitable for GL drawing.
 * soo the definition of GLXCreateWindow for a description of how
 * to call it.
 */

char *typeToName[] = {
    "color index single buffer",
    "color index double buffer",
    "rgb single buffer",
    "rgb double buffer",
};

/*
 * Dorky little helper function used to build up a GLXconfig array.
 */

static void set_entry (GLXconfig* ptr, int b, int m, int a)
{
    ptr->buffer = b;
    ptr->mode = m;
    ptr->arg = a;
}

/*
 * GLXCreateWindow(dpy, parent, x, y, w, h, boderWidth, type)
 *
 * Return value is the X window id of the newly created window.
 *
 * Arguments are:
 *	dpy		The X "Display*" returned by XOpenDisplay
 *	parent		The parent of the newly created window,
 *			a typical value for this is
 *			RootWindow(dpy, DefaultScreen(dpy))
 *	x,y		The location of the window to be created,
 *			y coordinate is measured from the top down.
 *	w,h		size of the new window
 *	borderWidth	the X border size for this window, should probably
 *			be zero.
 *	type		the GLXWindowType (see glxhelper.h) desribing the
 *			typer of GL drawing to be done in this window
 */
Window GLXCreateWindow(Display* dpy, Window parent, int x, int y, int w, int h,
		       int borderWidth, GLXWindowType type)
{
    GLXconfig params[50];
    GLXconfig* next;
    GLXconfig* retconfig;
    Colormap cmap = DefaultColormap(dpy, DefaultScreen(dpy));
    XVisualInfo* vis;
    XVisualInfo template;
    XColor white;
    XSetWindowAttributes cwa;
    XWindowAttributes	pwa;
    int i, nret;
    Window win;

    /*
     * This builds an array in "params" that describes for GLXgetconfig(3G)
     * the type of GL drawing that will be done.
     */
    next = params;
    switch (type) {
      case GLXcolorIndexSingleBuffer:
	set_entry(next++, GLX_NORMAL, GLX_RGB, FALSE);
	set_entry(next++, GLX_NORMAL, GLX_DOUBLE, FALSE);
	break;
      case GLXcolorIndexDoubleBuffer:
	set_entry(next++, GLX_NORMAL, GLX_RGB, FALSE);
	set_entry(next++, GLX_NORMAL, GLX_DOUBLE, TRUE);
	break;
      case GLXrgbSingleBuffer:
	set_entry(next++, GLX_NORMAL, GLX_RGB, TRUE);
	set_entry(next++, GLX_NORMAL, GLX_DOUBLE, FALSE);
	break;
      case GLXrgbDoubleBuffer:
	set_entry(next++, GLX_NORMAL, GLX_RGB, TRUE);
	set_entry(next++, GLX_NORMAL, GLX_DOUBLE, TRUE);
	break;
    }
    set_entry(next, 0, 0, 0); /* The input to GLXgetconfig is null terminated */

    /*
     * Get configuration data for a window based on above parameters
     * First we have to find out which screen the parent window is on,
     * then we can call GXLgetconfig()
     */
    XGetWindowAttributes(dpy, parent, &pwa);
    retconfig = GLXgetconfig(dpy, XScreenNumberOfScreen(pwa.screen), params);
    if (retconfig == 0) {
	printf("Sorry, can't support %s type of windows\n", typeToName[type]);
	exit(-1);
    }
    /*
     * The GL sets its own X error handlers, which aren't as informative
     * when errors happen.  Calling XSetErrorHandler(0) here will
     * reset back to the default Xlib version.
     */
    XSetErrorHandler(0);

    /*
     * Scan through config info, pulling info needed to create a window
     * that supports the rendering mode.
     */
    for (next = retconfig; next->buffer; next++) {
	unsigned long buffer = next->buffer;
	unsigned long mode = next->mode;
	unsigned long value = next->arg;
	switch (mode) {
	  case GLX_COLORMAP:
	    if (buffer == GLX_NORMAL) {
		cmap = value;
	    }
	    break;
	  case GLX_VISUAL:
	    if (buffer == GLX_NORMAL) {
		template.visualid = value;
		template.screen = DefaultScreen(dpy);
		vis = XGetVisualInfo(dpy, VisualScreenMask|VisualIDMask,
					  &template, &nret);
	    }
	    break;
	}
    }

    /*
     * Create the window
     */
    cwa.colormap = cmap;
    cwa.border_pixel = 0;  /* Even if we don't use it, it must be something */
    win = XCreateWindow(dpy, parent, x, y, w, h,
			     borderWidth, vis->depth, InputOutput, vis->visual,
			     CWColormap|CWBorderPixel, &cwa);

    /*
     * Rescan configuration info and find window slot that getconfig
     * provided.  Fill it in with the window we just created.
     */
    for (next = retconfig; next->buffer; next++) {
	if ((next->buffer == GLX_NORMAL) && (next->mode == GLX_WINDOW)) {
	    next->arg = win;
	    break;
	}
    }

    /*
     * Now "retconfig" contains all the information the GL needs to
     * configure the window and its own internal state.
     */
    i = GLXlink(dpy, retconfig);
    if (i < 0) {
	printf("GLXlink returned %d\n", i);
	exit(-1);
    }

    return win;
}
