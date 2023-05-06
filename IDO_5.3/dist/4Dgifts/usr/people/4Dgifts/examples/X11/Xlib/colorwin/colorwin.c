/*
 *    colorwin.c:
 *        
 *  First run:  This file is a sample X program designed to
 *  show how to start up a basic X window with a colored background.
 *
 *                 G. "Murdock" Helms, SGI Product Support - 6/11/90        
 */

#include <stdio.h>               /* have to include this to know about NULL */
#include <X11/Xlib.h>            /* Xlib include files */
#include <X11/Xutil.h>           /* Must have these included */

Display *display;
int screen;

main(argc, argv)
int argc;
char **argv;
{
    Window win, rootwin;
    XSizeHints sizehints;
    char *window_name = "Blue Background";
    char *icon_name = "blue";
    char *display_name = NULL;
    XGCValues values;
    Colormap cmap;               /* we'll use this for the background color */
    XColor greenish;
/* ignore the following two lines until you reach the program queue.c.
   You don't need to worry about these for now. */
    XEvent report;               /* will return our events */
    unsigned long valuemask;     /* null for now */



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

    rootwin = RootWindow(display, screen);
    win = XCreateSimpleWindow (display, rootwin, 0, 0, 200, 200, 5,
                               BlackPixel(display, screen),
                               WhitePixel(display, screen));   

/* set up the colormap for usage */
    cmap=DefaultColormap(display,screen);

/* Attempt to set background color for my window */
    XParseColor(display, cmap, "yellow green", &greenish);
    XAllocColor(display,cmap,&greenish);
    XSetWindowBackground(display, win, greenish.pixel); 

/* Again, ignore the following call.  This is queuing stuff, see
   queue.c for explanations */
    XSelectInput(display, win, ExposureMask | KeyPressMask |
                               ButtonPressMask | StructureNotifyMask);

/* Set properties for window manager (always before mapping!) 
    XSetStandardProperties(display, win, window_name, icon_name, None,
                           0, 0, &sizehints); */

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
