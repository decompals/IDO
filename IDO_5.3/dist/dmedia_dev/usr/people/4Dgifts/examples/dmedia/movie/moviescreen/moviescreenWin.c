/******************************************************************************
 *
 * File:        moviescreenWin.c 
 *
 * Description: Code for creating and accessing an  X window 
 *              suitable for GL rendering used by moviescreen.
 *
 *****************************************************************************/

#include <dmedia/moviefile.h>
#include <dmedia/movieplay.h>
#include "moviescreenWin.h"
#include "moviescreenArgs.h"
#include <stdio.h>
#include <X11/Xutil.h>
#include "glxhelper.h"

static Display	*dpy    = NULL;
static Window	win;

/*********
 *
 * Open a connection to the X server, and create a screen-sized X window
 * suitable for GL rendering here.
 *
 *********/

DMstatus createXWindow()
{
    XSetWindowAttributes childWinAttribs;
    unsigned long        black;
    XColor 		 c;
    int 		 scrn;

    /* 
     * Open a connection to the X server.
     */

    if ( dpy == NULL ) { 
        dpy = XOpenDisplay( 0 );
        if ( !dpy ) {
            fprintf( stderr, "%s: Cannot open display.\n", getProgramName() );
            return DM_FAILURE;
        }
    }

    /*
     * Get a pixel value for true black for the background pixel color.
     */

    c.red = c.green = c.blue = 0;
    if ( !XAllocColor( dpy, DefaultColormap( dpy,
                      DefaultScreen( dpy) ), &c ) ) {
        black = BlackPixel( dpy, scrn );
    } else {
        black = c.pixel;
    }

    /*
     * Fill in the desired window attributes.
     */

    childWinAttribs.colormap = DefaultColormap( dpy, DefaultScreen( dpy ) );
                                              
    /*
     * Even if we don't use it, it must be something.
     */

    childWinAttribs.border_pixel = 0;
    childWinAttribs.background_pixel = black;
    childWinAttribs.event_mask = StructureNotifyMask;
    childWinAttribs.override_redirect = True;

    /*
     * Create an X window configured for GL rendering, using
     * the helper functions defined in glxhelper.c
     */

    scrn = DefaultScreen( dpy );
    win = GLXCreateWindow( dpy, RootWindow( dpy, scrn ), 0, 0,
                           DisplayWidth( dpy, scrn ),
                           DisplayHeight( dpy, scrn ),
                           0, CWEventMask | CWOverrideRedirect |
                           CWBackPixel | CWColormap | CWBorderPixel,
                           &childWinAttribs, GLXrgbSingleBuffer );
    XStoreName( dpy, win, getProgramName() );

    return DM_SUCCESS;
}

/*********
 *
 * Return a pointer to the X Display.
 *
 *********/

Display *getXDisplay()
{
    return dpy;
}

/*********
 *
 * Return the background X Window id.
 *
 *********/

Window getXWindow()
{
    return win;
}
