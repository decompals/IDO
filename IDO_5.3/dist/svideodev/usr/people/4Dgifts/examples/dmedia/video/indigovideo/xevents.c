/*
 * xevents.c
 *
 * This X11 program displays live video from the IndigoVideo board and shows
 * how to decode X11 video-related event information.
 *
 * Hit the escape or the 'q' keys to exit.
 */

#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <svideo.h>		/* must be included after <X11/Xlib.h> */

/* We're interested in exposure, key and video-related events */
#define EVENTMASK   (ExposureMask|KeyPressMask|StructureNotifyMask)


main(int argc, char *argv[])
{
    Window          rootwin, win;
    Display        *display;
    XEvent          event;
    int             screen, width, height;
    SVhandle        V;
    long            param[2];


    /* Open connection to X server */
    if ((display = XOpenDisplay(0)) == NULL) {
	fprintf(stderr, "%s: cannot connect to X server", argv[0]);
	if (getenv("DISPLAY") == NULL)
	    fprintf(stderr,
		    ", `DISPLAY' environment variable not set.\n");
	else
	    fprintf(stderr, " %s\n", XDisplayName(0));
	exit(1);
    }

    /* Open video device */
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
    printf("Default window size: %d by %d\n", width, height);


    /* Create appropriate-sized window */
    screen = DefaultScreen(display);
    rootwin = RootWindow(display, screen);
    win = XCreateSimpleWindow(display, rootwin, 100, 100, width, height,
	       5, BlackPixel(display, screen), BlackPixel(display, screen));

    /* Set the window and icon names for the window manager before mapping it */
    XStoreName(display, win, "X Video Event Handler");
    XSetIconName(display, win, argv[0]);

    XSelectInput(display, win, EVENTMASK);
    XMapWindow(display, win);


    /* Associate video with window */
    if (svBindWindow(V, display, win, SV_IN_REPLACE) < 0) {
	svPerror("bindwindow");
	svCloseVideo(V);
	exit(1);
    }

    /* Receive video-related X events */
    svSelectXEvents(V, display);

    /* Event loop */
    while (1) {
	XNextEvent(display, &event);

	if (event.type == Expose) {
	    printf("Expose event\n");
	    if (svBindWindow(V, display, win, SV_IN_REPLACE) < 0) {
		svPerror("bindwindow");
		svCloseVideo(V);
		exit(1);
	    }
	} else if (event.type == KeyPress) {	/* See if we're done */
	    XKeyEvent      *kev = (XKeyEvent *) &event;
	    KeySym          keysym;
	    char            buf[4];

	    XLookupString(kev, buf, 1, &keysym, 0);
	    printf("Key pressed: '%c' (%d)\n", buf[0], buf[0]);
	    if (buf[0] == 'Q' || buf[0] == 'q' || buf[0] == '\033') {
		printf("Quitting...\n");
		svCloseVideo(V);
		exit(0);
	    }
	} else if (event.type == SvVideoActivityEventNumber) {
	    SVvideoActivityEvent *ev = (SVvideoActivityEvent *) & event;

	    if (ev->reason == SvVideoStarted) {
		printf("Video started\n");
	    } else if (ev->reason == SvVideoStopped) {
		printf("Video stopped\n");
	    } else if (ev->reason == SvVideoBusy) {
		printf("Video busy\n");
	    } else if (ev->reason == SvVideoPreempted) {
		printf("Lost video\n");
	    } else {
		printf("unknown video activity (%d)?\n", ev->reason);
	    }

	} else if (event.type == SvParamChangeEventNumber) {
	    SVparamChangeEvent *ev = (SVparamChangeEvent *) &event;

	    if (ev->attribute == SvActiveAttribute) {
		/* value always 0 */
		if (svBindWindow(V, display, win, SV_IN_REPLACE) < 0) {
		    svPerror("bindwindow");
		    svCloseVideo(V);
		    exit(1);
		}
		printf("Active attribute: re-bound video\n");
	    } else if (ev->attribute == SvEncodingAttribute) {
		printf("Encoding change: %d = ", ev->value);
		if (ev->value == SvNTSCComposite) {
		    printf("NTSC composite\n");
		} else if (ev->value == SvPALComposite) {
		    printf("PAL composite\n");
		} else if (ev->value == SvNTSCSVideo) {
		    printf("NTSC SVideo\n");
		} else if (ev->value == SvPALSVideo) {
		    printf("PAL SVideo\n");
		} else {
		    printf("?\n");
		}
	    } else if (ev->attribute == SvFreezeAttribute) {
		printf("Freeze attribute: %s\n", ev->value ? "on" : "off");
	    } else if (ev->attribute == SvSourceAttribute) {
		printf("Input source change: %d\n", ev->value + 1);
	    } else if (ev->attribute == SvParamChangeAttribute) {
		printf("Parameter changed\n");	/* value always 1 */
	    } else {
		printf("unknown param attribute (%d) ?\n", ev->attribute);
	    }
	}
    }
}
