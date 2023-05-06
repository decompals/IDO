/*
 * xrgbgrab.c
 *
 * This program captures and displays one frame of video from the
 * IndigoVideo board every time a mouse button is pressed. The RGB8 format
 * data are then displayed using X11 with an 8-bit TrueColor visual. 
 * This program demonstrates the IndigoVideo svCaptureOneFrame library routine.
 */

#ident "$Revision: 1.1 $"

#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <svideo.h>	/* must be included after <X11/Xlib.h> */

#define OPT_STR "d?"

static void
usage(char *name)
{
    printf("Usage: %s [options]\n", name);
    printf("\t-d	print debug messages\n");
}

/* Buffers for the raw and interleaved video data */
char fields[SV_PAL_XMAX * SV_PAL_YMAX];
char frame[SV_PAL_XMAX * SV_PAL_YMAX];

main(int argc, char *argv[])
{
    int             depth = 8, pad = 8;
    Window          rootwin, win;
    Display        *display;
    GC              wingc;
    XImage         *image;
    XVisualInfo     vinfo;
    Visual         *visual;
    XSetWindowAttributes attr;
    int             screen;
    int             debug, ch;
    int             width, height;
    SVhandle        V;
    long	    param[2];

    debug = FALSE;
    while ((ch = getopt(argc, argv, OPT_STR)) != -1) {
	switch (ch) {
	    case 'd': debug = TRUE;
	              break;
	    default:  usage(argv[0]);
	              exit(1);
	}
    }

    /* Open connection to X server */
    if ((display = XOpenDisplay(NULL)) == NULL) {
	fprintf(stderr, "%s: cannot connect to X server", argv[0]);
	if (getenv("DISPLAY") == NULL)
	    fprintf(stderr,
		    ", `DISPLAY' environment variable not set.\n");
	else
	    fprintf(stderr, " %s\n", XDisplayName(NULL));
	exit(1);
    }

    /* Open the video device */
    if ((V = svOpenVideo()) == NULL) {
	svPerror("open");
	exit(1);
    }

    /* Determine the window size from the signal standard */
    param[0] = SV_BROADCAST;
    svGetParam(V, param, 2);
    if (param[1] == SV_PAL) {
	width = SV_PAL_XMAX;
	height = SV_PAL_YMAX;
    } else {
	width = SV_NTSC_XMAX;
	height = SV_NTSC_YMAX;
    }
    if (debug) {
	printf("window size: %d by %d\n", width, height);
    }


    /* Create the appropriate-sized window */
    screen = DefaultScreen(display);
    rootwin = XRootWindow(display, screen);

    /*
     * Since we're displaying an 8bit RGB image,
     * we must use an 8bit TrueColor visual.
     */
    if (!XMatchVisualInfo(display, screen, depth, TrueColor, &vinfo)) {
	fprintf(stderr, "XMatchVisualInfo: can't get visual info\n");
	exit(1);
    }
    visual = vinfo.visual;

    attr.colormap = XCreateColormap(display, rootwin, visual, AllocNone);
    if ((win = XCreateWindow(display, rootwin, 200, 200, width, height,
			     0, depth, InputOutput, vinfo.visual, CWColormap,
			     &attr)) == NULL) {
	fprintf(stderr, "XCreateWindow: Error in creating simple window\n");
	exit(1);
    }
    /* Set the window and icon names for the window manager before mapping it */
    XStoreName(display, win, "X Video RGB Capture");
    XSetIconName(display, win, argv[0]);

    XMapWindow(display, win);

    wingc = XCreateGC(display, win, (unsigned long) 0, (XGCValues *) NULL);
    XSetFunction(display, wingc, GXcopy);

    /* Wait until the window is displayed before showing the data */
    XSelectInput(display, win, ExposureMask | KeyPressMask | ButtonPressMask);
    XSync(display, 0);
    for (;;) {
	XEvent          ev;

	XNextEvent(display, &ev);
	switch (ev.type) {
	    case KeyPress: {		/* See if we're done */
		    XKeyEvent *kev = (XKeyEvent *) &ev;
		    KeySym keysym;
		    char buf[4];

		    XLookupString(kev, buf, 1, &keysym, 0);
		    if (debug) {
			printf("key pressed: '%c' (%d)\n", buf[0], buf[0]);
		    }
		    if (buf[0] == 'Q' || buf[0] == 'q' || buf[0] == '\033') {
			exit(0);
		    }
		}
		break;

	    case Expose:
	    case ButtonPress:
		/* Get the data from the video board */
		if (svCaptureOneFrame
			    (V, SV_RGB8_FRAMES, &width, &height, fields) < 0) {
		    svPerror("capture 1 frame");
		    exit(1);
		}
		if (debug) {
		    printf("captured size: %d by %d\n", width, height);
		}
		svInterleaveFields(FALSE, fields, frame, width, height);

		/* Display the video data */
		image = XCreateImage(display, visual, depth, ZPixmap, 0,
			    frame, width, height, pad, width);
		XPutImage(display, win, wingc, image, 0, 0, 0, 0,
			    (unsigned int) width, (unsigned int) height);
		break;
	}
    }
}
