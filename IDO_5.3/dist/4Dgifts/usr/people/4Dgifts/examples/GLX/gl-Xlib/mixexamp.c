/*
 *    mixexamp.c: 
 *
 *   this program is the on-line version of what can be found at the end
 *   of chapter 4, section 4.4, "Mixed Model Example," of the 
 *   "IRIX 4D1-4.0 Transition Guide, Programming Environment"
 *
 *   This program is an example of a mixed model program that uses double 
 *   buffered RGB and overlays (or popups if no overlays are present), 
 *   based on Xlib.  
 *
 *       To compile:    cc -o mixexamp mixexamp.c -lgl_s -lX11_s
 *
 */

/* Include X headers files first */
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <gl/glws.h>

/* X Display */
Display* D;

/* To use for X screen */
int S;

struct {
    short	vertex[2];
    short	color[3];
} stamp[4] = {
	{ { 0, 0 }, { 255,   0,   0 } },
	{ { 0, 1 }, {   0, 255,   0 } },
	{ { 1, 1 }, {   0,   0, 255 } },
	{ { 1, 0 }, { 255, 255,   0 } }
};

/* Declare the data structure for the GL rendering configuration needed */
GLXconfig rgb_ov[] = {
	{ GLX_NORMAL,	GLX_RGB,	True} ,
	{ GLX_OVERLAY,	GLX_BUFSIZE,	2} ,
	{ 0,		0,		0}
};
/* use this one if we find we're on a machine that does not have overlays */
GLXconfig rgb_pup[] = {
	{ GLX_NORMAL,	GLX_RGB,	True} ,
	{ GLX_POPUP,	GLX_BUFSIZE,	2} ,
	{ 0,		0,		0}
};

unsigned long
extract_value(int buffer, int mode, GLXconfig *conf)
{
    int	i;
    for (i = 0; conf[i].buffer; i++)
	if (conf[i].buffer == buffer && conf[i].mode == mode)
	    return conf[i].arg;
    return 0;
}

/* Extract X visual information */
XVisualInfo*
extract_visual(int buffer, GLXconfig *conf)
{
    XVisualInfo	template, *v;
    int n;

    template.screen = S;
    template.visualid = extract_value(buffer, GLX_VISUAL, conf);
    return XGetVisualInfo (D, VisualScreenMask|VisualIDMask, &template, &n);
}

/* Fill the configuration structure with the appropriately */
/* created window */
void
set_window(int buffer, Window W, GLXconfig *conf)
{
    int	i;

    for (i = 0; conf[i].buffer; i++)
	if (conf[i].buffer == buffer && conf[i].mode == GLX_WINDOW)
	    conf[i].arg = W;
}


main(argc, argv)
int argc;
char *argv[];
{
    GLXconfig			*conf;
    XVisualInfo*		v;
    XSetWindowAttributes	attr;
    Window			W, ovW, wins[2];
    int				i;
    int				OverlayPlanes;


    /* Follow the DISPLAY environment variable */
    D = XOpenDisplay(0);
    S = DefaultScreen(D);

    if (getgdesc(GD_BITS_OVER_SNG_CMODE) < 2) {/* test for overlay planes */
        if ((conf = GLXgetconfig(D, S, rgb_pup)) == 0) {
    	    printf("getconfig failed\n");
	    exit(1);
        }
        OverlayPlanes = FALSE;
    } else {
        if ((conf = GLXgetconfig(D, S, rgb_ov)) == 0) {
    	    printf("getconfig failed\n");
	    exit(1);
        }
        OverlayPlanes = TRUE;
    }
	/* Turn off verbose Xlib error messages */
    XSetErrorHandler(0);

	/* Create main plane window */
    v = extract_visual(GLX_NORMAL, conf);
    attr.colormap = extract_value(GLX_NORMAL, GLX_COLORMAP, conf);
    attr.border_pixel = 0;
    W = XCreateWindow(D, RootWindow(D, S), 0, 0, 400, 400, 0,
			v->depth, InputOutput, v->visual,
			CWBorderPixel|CWColormap, &attr);
    XStoreName(D, W, "DBL-Bufr'd, Overlay/Popup Mix Model Window");
    set_window(GLX_NORMAL, W, conf);

	/* Create overlay window, or popup if no overlay on this machine */
    if (OverlayPlanes) {
        v = extract_visual(GLX_OVERLAY, conf);
	attr.colormap = extract_value(GLX_OVERLAY, GLX_COLORMAP, conf);
	ovW = XCreateWindow(D, W, 0, 0, 400, 400, 0,
			    v->depth, InputOutput, v->visual,
			    CWBorderPixel|CWColormap, &attr);
	set_window(GLX_OVERLAY, ovW, conf);
    } else {
        v = extract_visual(GLX_POPUP, conf);
	attr.colormap = extract_value(GLX_POPUP, GLX_COLORMAP, conf);
	ovW = XCreateWindow(D, W, 0, 0, 400, 400, 0,
			    v->depth, InputOutput, v->visual,
			    CWBorderPixel|CWColormap, &attr);
	set_window(GLX_POPUP, ovW, conf);
    }

	/* Bind the GL to the created windows */
    if (GLXlink(D, conf) < 0) {
	printf("Bind failed\n");
	exit(1);
    }

    wins[0] = W;
    wins[1] = ovW;
    XSetWMColormapWindows(D, W, wins, 2);
    XSelectInput(D, W, KeyPressMask|ExposureMask);
    XMapWindow(D, W);
    XSelectInput(D, ovW, ExposureMask);
    XMapWindow(D, ovW);
	
    for (;;) {
	XEvent	e;

	XNextEvent(D, &e);
	switch (e.xany.type) {
	    case Expose: {
		short v[2];
		/* Draw in main planes */
		if (GLXwinset(D, W) < 0) {
		    printf("winset failed\n");
		    exit(1);
		}
		reshapeviewport();
		ortho2(0, 1, 0, 1);
		bgnpolygon();
		for (i = 0; i < 4; i++) {
		    c3s(stamp[i].color);
		    v2s(stamp[i].vertex);
		}
		endpolygon();
		/* draw in overlays */
		if (GLXwinset(D, ovW) < 0) {
		    printf("winset failed\n");
		    exit(1);
		}
		color(0);
		rectf(0.0, 0.0, 1.0, 1.0);
		color(1);
		rectf(.2, .2, .8, .8);
		color(0);
		rectf(.4, .4, .6, .6);
		break;
	    }
/* Die on any keystroke */
	case KeyPress:
	    exit(0);
	}
    }
}
