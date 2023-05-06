/*
 *    placewin.c:
 *
 *  First run:  This file is a sample X program designed to
 *  show how to start up a basic X window in a specified position.
 *
 *                 G. "Murdock" Helms, SGI Product Support - 6/11/90
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
    Window win, rootwin;
    XSizeHints sizehints;
    char *window_name = "Placed Window";
    char *icon_name = "bwin";
    char *display_name = NULL;
    int x=100,y=100;
    unsigned int width=400, height=400;
/* ignore the following two lines until you reach the program queue.c.
   You don't need to worry about these for now. */
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
     * are macros.
     */

    rootwin = RootWindow(display, screen);
    win = XCreateSimpleWindow (display, rootwin, x, y, width, height, 5,
                               BlackPixel(display, screen),
                               WhitePixel(display, screen));

    /* note that this is for User Specified position */
    sizehints.flags=USPosition;             /* so it can be either/or */
    sizehints.x=x;                          /* this is part of XSizeHints */
    sizehints.y=y;                          /* see manual on structure of */

    /* Set properties for window manager (always before mapping!)  */
    XSetStandardProperties(display, win, window_name, icon_name, None,
                           0, 0, &sizehints); 

    /* Again, ignore the following call.  This is queuing stuff, see
       queue.c for explanations */
    XSelectInput(display, win, ExposureMask | KeyPressMask |
                               ButtonPressMask | StructureNotifyMask);
    
    /* Display window */
    XMapWindow(display, win);

/* Get events; use the first Expose to display text and graphics;
 * ConfigureNotify to indicate a resize;  ButtonPress or KeyPress to exit */
    while (1) {

        XNextEvent(display, &report);

        switch (report.type) {

        /* The window has been resized, so change width and height
         * for draw_text and draw_graphics in next Expose */
            case ConfigureNotify:
                width = report.xconfigure.width;
                height = report.xconfigure.height;
                break;

        /*Just trickle down into KeyPress (no break) */
            case ButtonPress:
            case KeyPress:

        /* Close down gracefully */
                XCloseDisplay(display);
                exit(1);

        /* All events selected by StructureNotifyMask except ConfigureNotify 
	 * are thrown away here, since nothing is done with them */
            default:
                break;

        }          /* end switch */
    }                           /* end while  */
}                                            /* end main   */
