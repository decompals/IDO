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
 * File: keyboard.c
 *
 * Description:
 *	Tests keyboard buttons. This is a mixed GL and X app. It is in
 *	mixed mode for two reasons:
 *
 *	1. Raw keycodes must be used to avoid the effects of users
 *	   xmodmaping their keyboards.
 *
 *	2. The keyboard must be grabbed to prevent the window manager
 *	   reacting to key combinations that are of interest to it.
 *
 **************************************************************************/


#ident "$Revision: 1.2 $"


#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <gl/glws.h>
#include <gl/gl.h>
#include <gl/device.h>


/* Screen dimensions */
#define XSCREENSIZE(d,s)	DisplayWidth(d,s)
#define YSCREENSIZE(d,s)	DisplayHeight(d,s)

/* Porgram window disred size */
#define	X_WIDTH	848
#define	Y_WIDTH	248

/* Color map info  - hard coded locations in GL default colormap */
#define	GREY1		8
#define GREY2		15
#define RED1		9

/* Grab actions */
#define GRAB_KEYS	0
#define UNGRAB_KEYS	1

/* Key types */
#define CONTROL 	1
#define ALPHANUM 	2

/* Keycode info */
#define GL_EXT		59		/* GL extended keycode offset */
#define X_KEYMAGIC	8		/* One byte offset in keycodes */
#define XKEY_TO_GLKEY(x)	((x) - X_KEYMAGIC + 1)

/* Key state test, set and clear macros */
#define TEST_KEYSTATE(key)	((key_states[(key+X_KEYMAGIC-1)/8] >> \
					((key+X_KEYMAGIC-1)%8)) & 1)
#define SET_KEYSTATE(key)	key_states[(key+X_KEYMAGIC-1)/8] |= \
					(1 << ((key+X_KEYMAGIC-1)%8))
#define CLR_KEYSTATE(key)	key_states[(key+X_KEYMAGIC-1)/8] &= \
					~(1 << ((key+X_KEYMAGIC-1)%8))

/* Drawing macros */
#define OUTLINE(a)  rect(a.center.x - a.size.x/2, a.center.y - a.size.y/2, \
		         a.center.x + a.size.x/2, a.center.y + a.size.y/2)

#define FILL(a)     rectf(a.center.x - a.size.x/2, a.center.y - a.size.y/2, \
		          a.center.x + a.size.x/2, a.center.y + a.size.y/2)

/* Misc defines */
#define NOTFOUND	-1

/* Key states */
#define KEY_UP		0
#define KEY_DOWN	1

/* Key lookup */
#define KEY_IND		0
#define KEY_CODE	1


typedef struct
{
    float x;
    float y;
} XY;

typedef struct 
{
    XY center;
    XY size;
} Area;

struct keyrect {
    int keyno;
    int keytype;
    Area keyarea;
    char *symbol;
};

struct lamprect {
    int lampno;
    int lamptype;
    Area lamparea;
};


static struct keyrect keys[] = {
	{ AKEY, ALPHANUM, { { 9.0,10.0}, { 4.0, 4.0} }, "A" },
	{ BKEY, ALPHANUM, { {27.0, 6.0}, { 4.0, 4.0} }, "B" },
	{ CKEY, ALPHANUM, { {19.0, 6.0}, { 4.0, 4.0} }, "C" },
	{ DKEY, ALPHANUM, { {17.0,10.0}, { 4.0, 4.0} }, "D" },
	{ EKEY, ALPHANUM, { {16.0,14.0}, { 4.0, 4.0} }, "E" },
	{ FKEY, ALPHANUM, { {21.0,10.0}, { 4.0, 4.0} }, "F" },
	{ GKEY, ALPHANUM, { {25.0,10.0}, { 4.0, 4.0} }, "G" },
	{ HKEY, ALPHANUM, { {29.0,10.0}, { 4.0, 4.0} }, "H" },
	{ IKEY, ALPHANUM, { {36.0,14.0}, { 4.0, 4.0} }, "I" },
	{ JKEY, ALPHANUM, { {33.0,10.0}, { 4.0, 4.0} }, "J" },
	{ KKEY, ALPHANUM, { {37.0,10.0}, { 4.0, 4.0} }, "K" },
	{ LKEY, ALPHANUM, { {41.0,10.0}, { 4.0, 4.0} }, "L" },
	{ MKEY, ALPHANUM, { {35.0, 6.0}, { 4.0, 4.0} }, "M" },
	{ NKEY, ALPHANUM, { {31.0, 6.0}, { 4.0, 4.0} }, "N" },
	{ OKEY, ALPHANUM, { {40.0,14.0}, { 4.0, 4.0} }, "O" },
	{ PKEY, ALPHANUM, { {44.0,14.0}, { 4.0, 4.0} }, "P" },
	{ QKEY, ALPHANUM, { { 8.0,14.0}, { 4.0, 4.0} }, "Q" },
	{ RKEY, ALPHANUM, { {20.0,14.0}, { 4.0, 4.0} }, "R" },
	{ SKEY, ALPHANUM, { {13.0,10.0}, { 4.0, 4.0} }, "S" },
	{ TKEY, ALPHANUM, { {24.0,14.0}, { 4.0, 4.0} }, "T" },
	{ UKEY, ALPHANUM, { {32.0,14.0}, { 4.0, 4.0} }, "U" },
	{ VKEY, ALPHANUM, { {23.0, 6.0}, { 4.0, 4.0} }, "V" },
	{ WKEY, ALPHANUM, { {12.0,14.0}, { 4.0, 4.0} }, "W" },
	{ XKEY, ALPHANUM, { {15.0, 6.0}, { 4.0, 4.0} }, "X" },
	{ YKEY, ALPHANUM, { {28.0,14.0}, { 4.0, 4.0} }, "Y" },
	{ ZKEY, ALPHANUM, { {11.0, 6.0}, { 4.0, 4.0} }, "Z" },

	{ ZEROKEY,  ALPHANUM, { {42.0,18.0}, { 4.0, 4.0} }, "0" },
	{ ONEKEY,   ALPHANUM, { { 6.0,18.0}, { 4.0, 4.0} }, "1" },
	{ TWOKEY,   ALPHANUM, { {10.0,18.0}, { 4.0, 4.0} }, "2" },
	{ THREEKEY, ALPHANUM, { {14.0,18.0}, { 4.0, 4.0} }, "3" },
	{ FOURKEY,  ALPHANUM, { {18.0,18.0}, { 4.0, 4.0} }, "4" },
	{ FIVEKEY,  ALPHANUM, { {22.0,18.0}, { 4.0, 4.0} }, "5" },
	{ SIXKEY,   ALPHANUM, { {26.0,18.0}, { 4.0, 4.0} }, "6" },
	{ SEVENKEY, ALPHANUM, { {30.0,18.0}, { 4.0, 4.0} }, "7" },
	{ EIGHTKEY, ALPHANUM, { {34.0,18.0}, { 4.0, 4.0} }, "8" },
	{ NINEKEY,  ALPHANUM, { {38.0,18.0}, { 4.0, 4.0} }, "9" },

	{ PAD0, ALPHANUM, { {80.0, 2.0}, { 8.0, 4.0} }, "0" },
	{ PAD1, ALPHANUM, { {78.0, 6.0}, { 4.0, 4.0} }, "1" },
	{ PAD2, ALPHANUM, { {82.0, 6.0}, { 4.0, 4.0} }, "2" },
	{ PAD3, ALPHANUM, { {86.0, 6.0}, { 4.0, 4.0} }, "3" },
	{ PAD4, ALPHANUM, { {78.0,10.0}, { 4.0, 4.0} }, "4" },
	{ PAD5, ALPHANUM, { {82.0,10.0}, { 4.0, 4.0} }, "5" },
	{ PAD6, ALPHANUM, { {86.0,10.0}, { 4.0, 4.0} }, "6" },
	{ PAD7, ALPHANUM, { {78.0,14.0}, { 4.0, 4.0} }, "7" },
	{ PAD8, ALPHANUM, { {82.0,14.0}, { 4.0, 4.0} }, "8" },
	{ PAD9, ALPHANUM, { {86.0,14.0}, { 4.0, 4.0} }, "9" },

	{ PADPERIOD,             ALPHANUM, { {86.0, 2.0}, { 4.0, 4.0} }, "." },
	{ PADMINUS,               CONTROL,  { {90.0,18.0}, { 4.0, 4.0} }, "-" },
	{ PADENTER,               CONTROL,  { {90.0, 4.0}, { 4.0, 8.0} }, "" },
	{ PADVIRGULEKEY - GL_EXT, CONTROL,  { {82.0,18.0}, { 4.0, 4.0} }, "/" },
	{ PADASTERKEY - GL_EXT,   CONTROL,  { {86.0,18.0}, { 4.0, 4.0} }, "*" },
	{ PADPLUSKEY - GL_EXT,    CONTROL,  { {90.0,12.0}, { 4.0, 8.0} }, "+" },
	
	{ LEFTCTRLKEY,           CONTROL, { { 3.0, 2.0}, { 6.0, 4.0} }, "Ctrl"},
	{ RIGHTCTRLKEY - GL_EXT, CONTROL, { {54.0, 2.0}, { 6.0, 4.0} }, "Ctrl"},
	{ LEFTALTKEY - GL_EXT,   CONTROL, { {11.0, 2.0}, { 5.0, 4.0} }, "Alt" },
	{ RIGHTALTKEY - GL_EXT,  CONTROL, { {47.0, 2.0}, { 5.0, 4.0} }, "Alt" },
	
	{ LEFTARROWKEY,  CONTROL, { {64.0, 2.0}, { 4.0, 4.0} }, "" },
	{ DOWNARROWKEY,  CONTROL, { {68.0, 2.0}, { 4.0, 4.0} }, "" },
	{ RIGHTARROWKEY, CONTROL, { {72.0, 2.0}, { 4.0, 4.0} }, "" },
	{ UPARROWKEY,    CONTROL, { {68.0, 6.0}, { 4.0, 4.0} }, "" },

	{ RIGHTSHIFTKEY, CONTROL, { {54.0, 6.0}, { 8.0, 4.0} }, "Shift" },
	{ LEFTSHIFTKEY,  CONTROL, { { 4.0, 6.0}, { 8.0, 4.0} }, "Shift" },
	
	{ F1KEY - GL_EXT,  ALPHANUM, { { 8.0,24.0}, { 4.0, 4.0} }, "" },
	{ F2KEY - GL_EXT,  ALPHANUM, { {12.0,24.0}, { 4.0, 4.0} }, "" },
	{ F3KEY - GL_EXT,  ALPHANUM, { {16.0,24.0}, { 4.0, 4.0} }, "" },
	{ F4KEY - GL_EXT,  ALPHANUM, { {20.0,24.0}, { 4.0, 4.0} }, "" },
	{ F5KEY - GL_EXT,  ALPHANUM, { {26.0,24.0}, { 4.0, 4.0} }, "" },
	{ F6KEY - GL_EXT,  ALPHANUM, { {30.0,24.0}, { 4.0, 4.0} }, "" },
	{ F7KEY - GL_EXT,  ALPHANUM, { {34.0,24.0}, { 4.0, 4.0} }, "" },
	{ F8KEY - GL_EXT,  ALPHANUM, { {38.0,24.0}, { 4.0, 4.0} }, "" },
	{ F9KEY - GL_EXT,  ALPHANUM, { {44.0,24.0}, { 4.0, 4.0} }, "" },
	{ F10KEY - GL_EXT, ALPHANUM, { {48.0,24.0}, { 4.0, 4.0} }, "" },
	{ F11KEY - GL_EXT, ALPHANUM, { {52.0,24.0}, { 4.0, 4.0} }, "" },
	{ F12KEY - GL_EXT, ALPHANUM, { {56.0,24.0}, { 4.0, 4.0} }, "" },

	{ LEFTBRACKETKEY,  ALPHANUM, { {48.0,14.0}, { 4.0, 4.0} }, "[" },
	{ RIGHTBRACKETKEY, ALPHANUM, { {52.0,14.0}, { 4.0, 4.0} }, "]" },

	{ HOMEKEY - GL_EXT,     CONTROL, { {68.0,18.0}, { 4.0, 4.0} }, "" },
	{ ENDKEY - GL_EXT,      CONTROL, { {68.0,14.0}, { 4.0, 4.0} }, "" },
	{ PAGEUPKEY - GL_EXT,   CONTROL, { {72.0,18.0}, { 4.0, 4.0} }, "" },
	{ PAGEDOWNKEY - GL_EXT, CONTROL, { {72.0,14.0}, { 4.0, 4.0} }, "" },
	{ INSERTKEY - GL_EXT,   CONTROL, { {64.0,18.0}, { 4.0, 4.0} }, "" },
	{ DELKEY,               CONTROL, { {64.0,14.0}, { 4.0, 4.0} }, "" },

	{ CAPSLOCKKEY,            CONTROL, { { 3.0,10.0}, { 6.0, 4.0} },"Caps"},
	{ NUMLOCKKEY - GL_EXT,    CONTROL, { {78.0,18.0}, { 4.0, 4.0} }, "" },
	{ SCROLLLOCKKEY - GL_EXT, CONTROL, { {68.0,24.0}, { 4.0, 4.0} }, "" },
	{ PRINTSCREENKEY - GL_EXT, CONTROL,  { {64.0,24.0}, { 4.0, 4.0} }, "" },
	{ PAUSEKEY - GL_EXT,       CONTROL,  { {72.0,24.0}, { 4.0, 4.0} }, "" },

	{ MINUSKEY,       ALPHANUM, { {46.0,18.0}, { 4.0, 4.0} }, "-" },
	{ EQUALKEY,       ALPHANUM, { {50.0,18.0}, { 4.0, 4.0} }, "=" },
	{ ESCKEY,         CONTROL,  { { 2.0,24.0}, { 4.0, 4.0} }, "" },
	{ TABKEY,         CONTROL,  { { 3.0,14.0}, { 6.0, 4.0} }, "Tab" },
	{ SEMICOLONKEY,   ALPHANUM, { {45.0,10.0}, { 4.0, 4.0} }, ";" },
	{ COMMAKEY,       ALPHANUM, { {39.0, 6.0}, { 4.0, 4.0} }, "<" },
	{ QUOTEKEY,       ALPHANUM, { {49.0,10.0}, { 4.0, 4.0} }, "'" },
	{ RETKEY,         CONTROL,  { {55.0,10.0}, { 8.0, 4.0} }, "Enter" },
	{ PERIODKEY,      ALPHANUM, { {43.0, 6.0}, { 4.0, 4.0} }, ">" },
	{ VIRGULEKEY,     ALPHANUM, { {47.0, 6.0}, { 4.0, 4.0} }, "/" },
	{ ACCENTGRAVEKEY, ALPHANUM, { { 2.0,18.0}, { 4.0, 4.0} }, "`" },
	{ BACKSLASHKEY,   ALPHANUM, { {56.0,14.0}, { 4.0, 4.0} }, "\\" },
	{ LINEFEEDKEY,    ALPHANUM, { {80.0, 2.0}, { 8.0, 4.0} }, "0" },
	{ BACKSPACEKEY,   CONTROL,  { {55.0,18.0}, { 6.0, 4.0} }, "BS" },
	{ SPACEKEY,       ALPHANUM, { {29.0, 2.0}, {30.0, 4.0} }, "Space" },
};

static struct lamprect lamps[] = 
{
	{ 1,  CONTROL, { {83.0,24.0}, {2.0,2.0} } },
	{ 2,  CONTROL, { {86.0,24.0}, {2.0,2.0} } },
	{ 3,  CONTROL, { {89.0,24.0}, {2.0,2.0} } }
};


static const int nkeys = sizeof(keys) / sizeof(struct keyrect);
static const int nlamps = sizeof(lamps) / sizeof(struct lamprect);

static char *progname = "keyboard";

static long xmin, xmax, ymin, ymax;	/* Screen extents */
static unsigned int screen_width, screen_height;
static Display *display;		/* The X server connection */
static int screen_num;			/* The X screen */
static Atom del_atom;			/* WM_DELETE_WINDOW atom */
static char key_states[32];		/* State of all keys on keyboard */


static void set_wm_hints(int, char**, Window, int, int, int, int);
static Window glx_create_window(Display*, Window, int, int, int, int, int);
static void trans(Area*, XY*);
static void shrink(Area*, float);
static int findbutton(int);
static void label_key(int);
static void makeframe();
static void draw_key(int, int, int);
static void clean_exit();
static void capslock_check();
static void smart_grab(int);


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
    Window top, glwin, windows[2];
    XEvent event;
    int got_expose;
    int keycode;

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
    xmin = (screen_width - X_WIDTH) / 2;
    xmax = screen_width - 1 - xmin;
    ymin = (screen_height - Y_WIDTH) / 2;
    ymax = screen_height - 1 - ymin;

    /*
     * Create a top level window
     */
    top = XCreateSimpleWindow(display, RootWindow(display, screen_num),
			      xmin, ymin, X_WIDTH, Y_WIDTH, 0, 0, 0);

    /*
     * Set window manager hints
     */
    set_wm_hints(argc, argv, top, xmin, ymin, X_WIDTH, Y_WIDTH);

    /*
     * Create a GL imaging window
     */
    if ((glwin = glx_create_window(display, top, 0, 0,
				   X_WIDTH, Y_WIDTH, 0)) == NULL) {
	fprintf(stderr, "%s: could not create GL window\n", progname);
	exit(1);
    }

    /*
     * Show interest in certain events
     */
    XSelectInput(display, top, StructureNotifyMask |
			       ButtonPressMask |
			       ButtonReleaseMask |
			       EnterWindowMask |
			       KeyPressMask |
			       KeyReleaseMask);
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
     * The event loop. The tricky thing here is keeping abreast of
     * the caps lock toggle key and doing the right thing for grabbing
     * the keyboard.
     */
    got_expose = FALSE;			/* No drawing until first expose */
    while (1) {
	gflush();			/* For proper DGL performance */
	XNextEvent(display, &event);
	switch (event.type) {
	    case EnterNotify:			/* Check caps lock on entry */
    		XQueryKeymap(display, key_states);
		capslock_check();
		break;
	    case Expose:			/* Exposures */
		if (event.xexpose.count)
		    continue; 
		got_expose = TRUE;
    		XQueryKeymap(display, key_states);
		makeframe();
		capslock_check();
		break;
	    case ConfigureNotify:		/* Resize GL manually */
		XResizeWindow(display, glwin, event.xconfigure.width,
			      event.xconfigure.height);
		XSync(display, False);		/* Need before GL reshape */
		viewport(0, event.xconfigure.width - 1,
			 0, event.xconfigure.height - 1);
		break;
	    case KeyPress:
		if (!got_expose)
		    break;
		keycode = XKEY_TO_GLKEY(event.xkey.keycode);
		/*
		 * Don't put a grab on if the key is already down (autorepeat)
		 */
		if ((keycode != CAPSLOCKKEY) &&
					(TEST_KEYSTATE(keycode) == 0)) {
		    smart_grab(GRAB_KEYS);
		    SET_KEYSTATE(keycode);
		}
		draw_key(keycode, KEY_CODE, KEY_DOWN);
		break;
	    case KeyRelease:
		if (!got_expose)
		    break;
		smart_grab(UNGRAB_KEYS);
		keycode = XKEY_TO_GLKEY(event.xkey.keycode);
		CLR_KEYSTATE(keycode);
		draw_key(keycode, KEY_CODE, KEY_UP);
		break;
	    case ButtonRelease:			/* Back door exit */
		if (event.xbutton.button == Button1)
		    clean_exit();
		break;
	    case ClientMessage:			/* WM invoked exit */
		if (event.xclient.data.l[0] == del_atom)
		    clean_exit();
		break;
	    default:				/* Blow off anything else */
		break;
	}
    }
}


/**************************************************************************
 *
 * Function:  glx_create_window
 *
 * Description: Creates a double single, color index X window suitable
 *	for GL imaging.
 *
 * Parameters: 
 *	dpy (I) - X display pointer
 *	parent (I) - parent window for the GL imaging window
 *	x, y (I) - window origin relative to parent
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
    	    { GLX_NORMAL, GLX_DOUBLE, FALSE },
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

static void set_wm_hints(int argc, char *argv[], Window top,
			 int x, int y, int w, int h)
{
    XTextProperty windowName;
    XSizeHints size_hints;
    XClassHint class_hints;
    XWMHints wm_hints;

    /*
     * Talk to any window manager
     */
    size_hints.flags = USPosition | PBaseSize;
    size_hints.x = x;
    size_hints.y = y;
    size_hints.base_width = w;
    size_hints.base_height = h;

    if (XStringListToTextProperty(&progname, 1, &windowName) == 0) {
	fprintf(stderr, "%s: structure allocation for windowName failed.\n",
		progname);
	exit(1);
    }

    wm_hints.initial_state = NormalState;
    wm_hints.input = True;
    wm_hints.flags = StateHint | InputHint;

    class_hints.res_name = progname;
    class_hints.res_class = "Keyboard";

    XSetWMProperties(display, top, &windowName, NULL, argv, argc,
		     &size_hints, &wm_hints, &class_hints);

    /*
     * Show interest in WM killing my app
     */
    if ((del_atom = XInternAtom(display, "WM_DELETE_WINDOW", True)) != None)
	XSetWMProtocols(display, top, &del_atom, 1);
}


/**************************************************************************
 *
 * Function: trans
 *
 * Description: Translates an area
 *
 * Parameters: 
 *	a (I) - area to translate
 *	p (I) - amount to translate it
 *
 * Return: none
 *
 **************************************************************************/

static void trans(Area *a, XY *p)
{
    a->center.x += p->x;
    a->center.y += p->y;
}


/**************************************************************************
 *
 * Function: shrink
 *
 * Description: Shrinks an area the same amount in both x and y.
 *
 * Parameters: 
 *	a (I) - area to shrink
 *	amount (I) - amount ot shrink it
 *
 * Return: none
 *
 **************************************************************************/

static void shrink(Area *a, float amount)
{
    a->size.x -= amount;
    a->size.y -= amount;
}


/**************************************************************************
 *
 * Function: findbutton
 *
 * Description: Linearly searches through the button array looking for
 *	 the specified keycode.
 *
 * Parameters: 
 *	keycode (I) - keycode to search for
 *
 * Return: Index of the key in the array of keys or NOTFOUND.
 *
 **************************************************************************/

static int findbutton(int keycode)
{
    register int i;

    for (i = 0; i < nkeys; i++) {
        if (keys[i].keyno == keycode)
	    return i;
    }
    return NOTFOUND;
}


/**************************************************************************
 *
 * Function: draw_key
 *
 * Description: Draws and labels a key
 *
 * Parameters: 
 *	keyval (I) - either a keycode or an index into the key array
 *	lookup (I) - indicates whether keyval is a KEY_CODE or a KEY_IND
 *	state (I) - state in which to draw key (KEY_UP, KEY_DOWN)
 *
 * Return: none
 *
 **************************************************************************/

static void draw_key(int keyval, int lookup, int state)
{
    static XY shadelta = { 0.4, -0.4 };
    Area shadow, keytop;
    int keyno;

    /*
     * See if we need to look up the key
     */
    if (lookup == KEY_CODE) {
        if ((keyno = findbutton(keyval)) == NOTFOUND)
	    return;
    }
    else {
	keyno = keyval;
    }

    /*
     * Create the shadow
     */
    shadow = keys[keyno].keyarea;
    shrink(&shadow, 0.8);
    trans(&shadow, &shadelta);

    /*
     * Draw shadow and pick color for key top based on state
     */
    switch (state) {
	case KEY_UP:
	    color(GREY1);
	    FILL(shadow);
	    if (keys[keyno].keytype == ALPHANUM)
	        color(WHITE);
	    else
	        color(GREY2);
	    break;
	case KEY_DOWN:
	    color(GREY2);
	    FILL(shadow);
	    color(GREY1);
	    break;
    }

    /*
     * Draw keytop
     */
    keytop = keys[keyno].keyarea;
    shrink(&keytop, 0.8);
    FILL(keytop);
    color(BLACK);
    OUTLINE(keytop);

    /*
     * Label the key
     */
    label_key(keyno);
}


/**************************************************************************
 *
 * Function: label_key
 *
 * Description: Draws the text on a key
 *
 * Parameters: 
 *	keyno (I) - index of key in key array
 *
 * Return: none
 *
 **************************************************************************/

static void label_key(int keyno)
{
    if (*keys[keyno].symbol) {		/* Only draw if there is a label */
        color(RED);
        cmov2(keys[keyno].keyarea.center.x - keys[keyno].keyarea.size.x/2 + 1,
	      keys[keyno].keyarea.center.y);
        charstr(keys[keyno].symbol);
    }
}


/**************************************************************************
 *
 * Function: makeframe
 *
 * Description: Draws the entire keyboard image including buttons and
 *	 labels.
 *
 * Parameters: none
 *
 * Return: none
 *
 **************************************************************************/

static void makeframe()
{
    int i;
    static int firsted = 0;

    /*
     * Define world coords and a few other things on first time through
     */
    if (!firsted) {
	ortho2(-10.0, 96.0, -4.0, 27.0);
	for (i = 0; i < nlamps; i++)	/* Shrink led area for line width */
	    shrink(&lamps[i].lamparea, 0.8);
	firsted = 1;
    }

    /*
     * Main background and line width
     */
    color(GREY2);
    clear();
    linewidth(2);

    /*
     * Draw LED lamps
     */
    for (i = 0; i < nlamps; i++) {
        color(RED1);
        FILL(lamps[i].lamparea);
        color(BLACK);
        OUTLINE(lamps[i].lamparea);
    }

    /*
     * Draw all keys
     */
    for (i = 0; i < nkeys; i++)
        draw_key(i, KEY_IND, KEY_UP);

    /*
     * Tell user about back door exit
     */
    color(BLACK);
    cmov2i(-8.0, -3.0);
    charstr("Use left mouse button to quit");
}


/**************************************************************************
 *
 * Function: capslock_check
 *
 * Description: Tests the state of the caps lock toggle key and draws
 *	 the key in the appropriate state
 *
 * Parameters: none
 *
 * Return: none
 *
 **************************************************************************/

static void capslock_check()
{
    if (TEST_KEYSTATE(CAPSLOCKKEY))
        draw_key(CAPSLOCKKEY, KEY_CODE, KEY_DOWN);
    else
        draw_key(CAPSLOCKKEY, KEY_CODE, KEY_UP);
}


/**************************************************************************
 *
 * Function: smart_grab
 *
 * Description: Uses reference count to initiate and clear keyboard
 *	grabs.
 *
 * Parameters: 
 *	action (I) - one of GRAB_KEYS, UNGRAB_KEYS
 *
 * Return: none
 *
 **************************************************************************/

static void smart_grab(int action)
{
    static ngrabs = 0;

    switch (action) {

	/*
	 * Always allow grabbing of the keyboard. Each grab increments
	 * the reference count
	 */
	case GRAB_KEYS:
	    ngrabs++;
            XGrabKeyboard(display, RootWindow(display, screen_num),
			  False, GrabModeAsync, GrabModeAsync,
			  CurrentTime);
	    break;

	/*
	 * Only when the reference count is 0 or less will we release the
	 * keyboard. Set ref count to zero in case it somehow got negative
	 */
	case UNGRAB_KEYS:
	    if (--ngrabs <= 0) {
                XUngrabKeyboard(display, CurrentTime);
		ngrabs = 0;
	    }
	    break;
    }
}


/**************************************************************************
 *
 * Function: clean_exit
 *
 * Description: Ties off loose ends before exiting the program
 *
 * Parameters: none
 *
 * Return: none
 *
 **************************************************************************/

static void clean_exit()
{
    XUngrabKeyboard(display, CurrentTime);	/* Just in case hanging grab */
    XCloseDisplay(display);
    exit(0);
}
