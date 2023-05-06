/**************************************************************************
 *									  *
 * 		 Copyright (C) 1991, Silicon Graphics, Inc.		  *
 * 			All Rights Reserved.				  *
 *									  *
 * This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.; *
 * the contents of this file may not be disclosed to third parties,	  *
 * copied or duplicated in any form, in whole or in part, without the	  *
 * prior written permission of Silicon Graphics, Inc.			  *
 *									  *
 * RESTRICTED RIGHTS LEGEND:						  *
 *	Use, duplication or disclosure by the Government is subject to    *
 *	restrictions as set forth in subdivision (c)(1)(ii) of the Rights *
 *	in Technical Data and Computer Software clause at DFARS 	  *
 *	252.227-7013, and/or in similar or successor clauses in the FAR,  *
 *	DOD or NASA FAR Supplement. Unpublished - rights reserved under   *
 *	the Copyright Laws of the United States.			  *
 **************************************************************************
 *
 * File: mouse.c
 *
 * Description:
 *	Tests mouse motion and buttons. This is a mixed GL and X app.
 *	It is in mixed mode so that the button events can be grabbed
 *	away from the window manager. Thus, if someone has done something
 *	like remapping mouse buttons so that they mean something to the
 *	WM inside an apps window, the confidence test will still get
 *	the button events.
 *
 **************************************************************************/


#ident "$Revision: 1.2 $"


#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#define XK_MISCELLANY			/* Needed for defininition of Esc */
#include <X11/keysymdef.h>
#include <Xm/MwmUtil.h>
#include <gl/glws.h>
#include <gl/gl.h>


/* Screen dimensions */
#define XSCREENSIZE(d,s)	DisplayWidth(d,s)
#define YSCREENSIZE(d,s)	DisplayHeight(d,s)

/* World coordinate space */
#define WORLD_XMIN	0.0
#define WORLD_XMAX	100.0
#define WORLD_YMIN	0.0
#define WORLD_YMAX	100.0

/* Color map info */
#define	GREY1		8		/* Greys in default GL colormap */
#define GREY2		15

/* Misc info */
#define MOUSE_OBJ 	1		/* Mouse object token */
#define LEN		300.0		/* Mouse cord length */


static char *progname = "mouse";

static Matrix C_spline = {		/* Mouse cord spline basis */
    {-0.5,  1.5, -1.5,  0.5},
    { 1.0, -2.5,  2.0, -0.5},
    {-0.5,  0.0,  0.5,  0.0},
    { 0.0,  1.0,  0.0,  0.0},
};

static float geom[4][3] = {		/* Mouse cord spline coords */
    { WORLD_XMIN,        6.0-LEN, 0.0 },
    {        0.0,            6.0, 0.0 },
    { WORLD_XMIN,     WORLD_YMAX, 0.0 },
    {        0.0, WORLD_YMAX+LEN, 0.0 },
};

static long xmin, xmax, ymin, ymax;	/* Screen extents */
static unsigned int screen_width, screen_height;
static float xrat, yrat;		/* World to screen mapping ratios */
static Display *display;		/* The X server connection */
static int screen_num;			/* The X screen */


static void set_wm_hints(int, char**, Window);
static Window glx_create_window(Display*, Window, int, int, int, int, int);
static void define_geom();
static void draw_screen(int, int, unsigned int);


/**************************************************************************
 *
 * Function: main
 *
 * Description: Program entry point
 *
 * Parameters: 
 *	argc (I) - command-line argument count
 *	argv (I) - command-line arguments
 *
 * Return: none
 *
 **************************************************************************/

void main(int argc, char *argv[])
{
    Window top, glwin, root, child, windows[2];
    XEvent event;
    char buffer[5];
    int got_expose;
    int x, y, root_x, root_y;
    unsigned int keys_buttons;
    KeySym keysym;
    XComposeStatus compose;

    /*
     * Connect to the X server and get screen info
     */
    if ((display = XOpenDisplay(NULL)) == NULL) {
	fprintf(stderr, "%s: cannot connect to X server %s\n",
		progname, XDisplayName(NULL));
	exit(1);
    }
    screen_num = DefaultScreen(display);
    screen_width = XSCREENSIZE(display, screen_num);
    screen_height = YSCREENSIZE(display, screen_num);

    /*
     * Set up screen coordinates
     */
    xmin = 0;
    xmax = screen_width - 1;
    ymin = 0;
    ymax = screen_height - 1;
    xrat = (WORLD_XMAX - WORLD_XMIN) / (float)(xmax - xmin);
    yrat = (WORLD_YMAX - WORLD_YMIN) / (float)(ymax - ymin);

    /*
     * Create a top level window
     */
    top = XCreateSimpleWindow(display, RootWindow(display, screen_num),
			      0, 0, screen_width, screen_height, 0, 0, 0);

    /*
     * Set window manager hints
     */
    set_wm_hints(argc, argv, top);

    /*
     * Create a GL imaging window
     */
    if ((glwin = glx_create_window(display, top, 0, 0,
				   screen_width, screen_height, 0)) == NULL) {
	fprintf(stderr, "%s: could not create GL window\n", progname);
	exit(1);
    }


    /*
     * Show interest in certain events
     */
    XSelectInput(display, top, KeyPressMask |
			       ButtonPressMask |
			       ButtonReleaseMask |
			       PointerMotionMask);
    XSelectInput(display, glwin, ExposureMask);

    /*
     * We need to ensure that the GL colormap is installed
     * for this app
     */
    windows[0] = glwin;
    windows[1] = top;
    XSetWMColormapWindows(display, top, windows, 2);

    /*
     * Finally, map the windows
     */
    XMapWindow(display, glwin);
    XMapWindow(display, top);

    /*
     * Set GL imaging
     */
    if (GLXwinset(display, glwin) < 0) {
        fprintf(stderr, "%s: could not winset GL window\n", progname);
        exit(1);
    }

    /*
     * Define the app geometry
     */
    define_geom();

    /*
     * Grab the mouse buttons so that we are sure to get
     * all mouse button events
     */
    XGrabButton(display, AnyButton, AnyModifier,
		RootWindow(display, screen_num), False,
		ButtonPressMask | ButtonReleaseMask | PointerMotionMask,
		GrabModeAsync, GrabModeAsync,
		None, None);

    /*
     * The event loop. Nothing fancy, just wait until a lull in
     * the event storm to do any drawing.
     */
    got_expose = FALSE;			/* No drawing until first expose */
    while (1) {
	gflush();			/* For proper DGL performance */
	XNextEvent(display, &event);
	switch (event.type) {
	    case Expose:			/* Exposures */
		if (event.xexpose.count)
		    continue; 
		got_expose = TRUE;
		break;
	    case MotionNotify:			/* Any mouse state change */
	    case ButtonPress:
	    case ButtonRelease:
		break;
	    case KeyPress:			/* Escape to quit app */
		XLookupString(&event, buffer, sizeof(buffer),
			      &keysym, &compose);
		if (keysym == XK_Escape) {
    		    XCloseDisplay(display);
    		    exit(0);
		}
		continue;
	    default:				/* Blow off anything else */
		continue;
	}

	/*
	 * If no events left and we have our first expose, draw the rodent
	 */
	if (!XEventsQueued(display, QueuedAlready) && got_expose) {
    	    XQueryPointer(display, glwin, &root, &child, &root_x, &root_y,
			  &x, &y, &keys_buttons);
	    draw_screen(x, y, keys_buttons);
	}
    }
}


/**************************************************************************
 *
 * Function:  glx_create_window
 *
 * Description: Creates a double buffered, color index X window suitable
 *	for GL imaging.
 *
 * Parameters: 
 *	dpy (I) - X display pointer
 *	parent (I) - parent window for the GL imaging window
 *	x, y (I) - window origin
 *	w, h (I) - window width and height
 *	boerderWidth (I) - number of pixels wide for border
 *
 * Return: the created GL imaging window
 *
 **************************************************************************/

static Window glx_create_window(Display* dpy, Window parent, int x, int y,
			 	int w, int h, int borderWidth)
{
    static GLXconfig params[] = {
    	    { GLX_NORMAL, GLX_RGB, FALSE },
    	    { GLX_NORMAL, GLX_DOUBLE, TRUE },
    	    { 0, 0, 0 },
	    };
    GLXconfig *next, *retconfig;
    Colormap cmap = DefaultColormap(dpy, DefaultScreen(dpy));
    XVisualInfo* vis;
    XVisualInfo template;
    XSetWindowAttributes cwa;
    int nret;
    Window win;

    /*
     * Get configuration data for a window based on above parameters
     */
    if ((retconfig = GLXgetconfig(dpy, DefaultScreen(dpy), params)) == NULL) {
	fprintf(stderr, "%s: cannot support window type\n", progname);
	exit(1);
    }

    /*
     * Scan through config info, pulling info needed to create a window
     * that supports the rendering mode.
     */
    for (next = retconfig; next->buffer; next++) {
	if (next->buffer == GLX_NORMAL) {
	    if (next->mode == GLX_COLORMAP) {
	        cmap = next->arg;
	    }
	    else if (next->mode == GLX_VISUAL) {
	        template.visualid = next->arg;
	        template.screen = DefaultScreen(dpy);
	        vis = XGetVisualInfo(dpy, VisualScreenMask | VisualIDMask,
					  &template, &nret);
	    }
	}
    }

    /*
     * Create the window
     */
    cwa.colormap = cmap;
    cwa.border_pixel = 0;
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
     * Link to the GL
     */
    if (GLXlink(dpy, retconfig) < 0) {
	fprintf(stderr, "%s: could not link with the GL\n", progname);
	exit(1);
    }

    return win;
}


/**************************************************************************
 *
 * Function: set_wm_hints
 *
 * Description: Sets the window manager hints for things like window
 *	size, placement and decoration.
 *
 * Parameters: 
 *	argc (I) - command-line argument count
 *	argv (I) - command-line arguments
 *	top (I) - top level window
 *
 * Return: none
 *
 **************************************************************************/

static void set_wm_hints(int argc, char *argv[], Window top)
{
    XTextProperty windowName;
    XSizeHints size_hints;
    XClassHint class_hints;
    XWMHints wm_hints;
    MotifWmHints mwm_hints;
    Atom mwm_hints_atom;

    /*
     * Talk to any window manager
     */
    size_hints.flags = USPosition | PBaseSize | PMaxSize;
    size_hints.x = 0;
    size_hints.y = 0;
    size_hints.base_width = screen_width;
    size_hints.base_height = screen_height;
    size_hints.max_width = screen_width;
    size_hints.max_height = screen_height;

    if (XStringListToTextProperty(&progname, 1, &windowName) == 0) {
	fprintf(stderr, "%s: structure allocation for windowName failed.\n",
		progname);
	exit(1);
    }

    wm_hints.initial_state = NormalState;
    wm_hints.input = True;
    wm_hints.flags = StateHint | InputHint;

    class_hints.res_name = progname;
    class_hints.res_class = "Mouse";

    XSetWMProperties(display, top, &windowName, NULL, argv, argc,
		     &size_hints, &wm_hints, &class_hints);

    /*
     * Talk to mwm if it is running
     */
    mwm_hints_atom = XInternAtom(display, _XA_MOTIF_WM_HINTS, False);
    mwm_hints.flags = MWM_HINTS_DECORATIONS | MWM_HINTS_FUNCTIONS;
    mwm_hints.decorations = 0;			/* No decorations */
    mwm_hints.functions = MWM_FUNC_CLOSE;	/* Only allow close */
    XChangeProperty(display, top, mwm_hints_atom, mwm_hints_atom,
		    32, PropModeReplace, &mwm_hints,
		    PROP_MWM_HINTS_ELEMENTS);
}


/**************************************************************************
 *
 * Function: draw_screen
 *
 * Description: Draws the application screen including the mouse pointer
 *	object.
 *
 * Parameters: 
 *	x, y (I) - screen coordinates of the mouse. Note, GL y=0 is at
 *		   bottom of screen and X y=0 is at top. 
 *	keys_buttons (I) - mouse button state
 *
 * Return: none
 *
 **************************************************************************/

static void draw_screen(int x, int y, unsigned int keys_buttons)
{
    float xoff, yoff;
    static int first_time = TRUE;

    /*
     * Image into both buffers the first time
     */
    if (first_time) {
	frontbuffer(TRUE);
	backbuffer(TRUE);
    }

    /*
     * Clear the background
     */
    color(GREY2);
    clear();
    color(BLACK);

    /*
     * Write exit message
     */
    cmov2i(2, 10);
    charstr("Use Esc key to quit.");

    /*
     * Draw mouse cord
     */
    xoff = (float)(x - xmin) * xrat;
    yoff = (float)(ymax - ymin - y) * yrat;
    geom[0][1] = yoff + 6.0 - LEN;
    geom[1][0] = xoff;
    geom[1][1] = yoff + 6.0; 
    geom[3][0] = xoff;
    crv(geom);

    /*
     * Draw mouse itself
     */
    pushmatrix();
        translate(xoff,yoff,0.0);
        callobj(MOUSE_OBJ);
        if (keys_buttons & Button1Mask)
            rectf(-2.75,0.0,-1.25,4.5);
 	if (keys_buttons & Button2Mask)
     	    rectf(-0.75,0.0, 0.75,4.5);
 	if (keys_buttons & Button3Mask)
     	    rectf( 1.25,0.0, 2.75,4.5);
    popmatrix();

    /*
     * Do the right thing for the first and subsequent draws
     */
    if (first_time) {
	frontbuffer(FALSE);
        first_time = FALSE;
	}
    else
        swapbuffers();
}


/**************************************************************************
 *
 * Function: define_geom
 *
 * Description: Defines the world to screen transformations, the mouse
 *	cord geometry and the mouse object geometry.
 *
 * Parameters: none
 *
 * Return: none
 *
 **************************************************************************/

static void define_geom()
{
    /*
     * Define world/screen transformation and mouse cord curve stuff
     */
    ortho2(WORLD_XMIN, WORLD_XMAX, WORLD_YMIN, WORLD_YMAX);
    defbasis(100,C_spline);
    curveprecision(20);
    curvebasis(100);

    /*
     * Define mouse geometry
     */
    makeobj(MOUSE_OBJ);
	color(WHITE);
	rectf(-4.0,-6.0,4.0,6.0);
	color(BLACK);
	rect(-4.0,-6.0,4.0,6.0);
	rect(-2.75,0.0,-1.25,4.5);
	rect(-0.75,0.0, 0.75,4.5);
	rect( 1.25,0.0, 2.75,4.5);
	color(GREY1);
	rectf(-3.0,-6.0,5.0,-7.0);
	rectf(4.0,-7.0,5.0,5.0);
    closeobj();
}

