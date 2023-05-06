/*
 *    simplewin.c:
 * 
 *  First run:  This file is a sample X program designed to show how to 
 *  start up the most basic window going.  That's all it does, too.
 *
 *                 G. "Murdock" Helms, SGI Product Support - 5/8/90
 */

#include <stdio.h>               /* have to include this to know about NULL */
#include <X11/Xlib.h>            /* Xlib include files */
#include <X11/Xutil.h>           /* Must have these included */

/* Display and screen are used as arguments to nearly every Xlib
 * routine, so it simplifies things to declare them global. */

Display *display;
int screen;

main(argc,argv)
int argc;
char **argv;
{
    Window win, rootwin;        /* both of these lines get set later */
    XSizeHints size_hints;
    char *window_name = "Your Basic Window Program";
    char *icon_name = "bwin";
    char *display_name = NULL;  /* since we haven't set anything yet */
/*  Ignore the next two lines.  It has to do with queing up
    the Esc. key.  See queue.doc for more info on this sort of thing.  */
    XEvent report;              /* will return our events */
    unsigned long valuemask;    /* null for now */



/* Connect to  X server ... this is kind of a fancy test to make
 * sure the server is running, but it produces a nicer error code
 * than you normally get. */

    if ( (display=XOpenDisplay(display_name)) == NULL ) {
	fprintf( stderr, "%s: cannot connect to X server", argv[0]);
	if (getenv("DISPLAY") == NULL)
            fprintf( stderr, ", 'DISPLAY' environment variable not set.\n");
        else
            fprintf( stderr, " %s\n", XDisplayName(display_name));
        exit ( -1 );
    }

    /* Get screen from display structure macro */
    screen = DefaultScreen(display);

    /* Create opaque window ... note that BlackPixel and WhitePixel
     * are macros.  */

    rootwin = RootWindow(display, screen);
    win = XCreateSimpleWindow (display, rootwin, 100, 100, 200, 150, 5,
                               BlackPixel(display, screen),
                               WhitePixel(display, screen));

    /* Set properties for window manager before mapping it */
    XSetStandardProperties(display, win, window_name, icon_name, None,
                           0, 0, &size_hints);

/*  More queuing stuff.  Ignore it.  */
    XSelectInput(display, win, ExposureMask | KeyPressMask |
                               ButtonPressMask | StructureNotifyMask);
    
    /* Put window up */
    XMapWindow(display, win);

/* Get events; use the first Expose to display text and graphics;
 * ConfigureNotify to indicate a resize;  ButtonPress or KeyPress to exit */
    while (1) {

        XNextEvent(display, &report);

        switch (report.type) {

            /*Just trickle down into KeyPress (no break) */
            case ButtonPress:
            case KeyPress:

            /* Close down gracefully */
                XCloseDisplay(display);
                exit(1);        

            /* All events selected by StructureNotifyMask except
             * ConfigureNotify are thrown away here, since
             * nothing is done with them */
            default:
                break;

        }          /* end switch */
    }                           /* end while  */
}                                            /* end main   */
