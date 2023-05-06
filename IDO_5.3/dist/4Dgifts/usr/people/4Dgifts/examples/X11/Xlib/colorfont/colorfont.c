/*
 *    colorfont.c:
 *
 *  First run:  This file is a sample X program designed to
 *  show how to use fonts and change their color.
 *
 *                 G. "Murdock" Helms, SGI Product Support - 10/30/90        
 */

#include <stdio.h>               /* have to include this to know about NULL */
#include <string.h>
#include <X11/Xlib.h>            /* Xlib include files */
#include <X11/Xutil.h>           /* Must have these included */

/* Display and screen are used as arguments to nearly every Xlib
 * routine, so it simplifies things to declare them global. */

Display *display;
int screen;
Window win;

main(argc, argv)
int argc;
char **argv;
{
    Window rootwin;
    XSizeHints sizehints;
    char *window_name = "Colored Fonts";
    char *icon_name = "More fonts";
    char *display_name = NULL;
    int x=100,y=100;
    unsigned int width=400, height=400;
    XFontStruct *font_info;                /* needed for font stuff */
    GC gc;                                 /* needed to do text output */
/*  Ignore the next two lines.  It has to do with queing up
    the Esc. key.  See queue.doc for more info on this sort of thing.  */
    XEvent report;                         /* will return our events */
    unsigned long valuemask;               /* null for now */



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


/* Make a black window this time so we can see colors clearly.  */
    rootwin = RootWindow(display, screen);
    win = XCreateSimpleWindow (display, rootwin, x, y, width, height, 5,
                               BlackPixel(display, screen),
                               BlackPixel(display, screen));

/* Check that the font you want is actually there and set it
 * to something useable. */
   load_my_font(&font_info);

/* Still need a GC for fonts */
   get_GC(win,&gc,font_info);

/* Set properties for window manager (always before mapping!)  */
    XSetStandardProperties(display, win, window_name, icon_name, None,
                           0, 0, &sizehints); 

/*  More queuing stuff.  Ignore it.  */
    XSelectInput(display, win, ExposureMask | KeyPressMask |
                               ButtonPressMask | StructureNotifyMask);
    
/* Display window */
    XMapWindow(display, win);

/* Print out text stuff */
   draw_text(win, gc, font_info);

/* Get events; use the first Expose to display text and graphics;
 * ConfigureNotify to indicate a resize;  ButtonPress or KeyPress to exit */
    while (1) {

        XNextEvent(display, &report);

        switch (report.type) {

	/* Draw our text on first Expose */
	    case Expose:
	      draw_text(win, gc, font_info);
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



load_my_font(font_info)
XFontStruct **font_info;
{
   char *fontname = "8x16romankana";  /* it looked kinda neat */


/*  Access the font  */
   if ((*font_info = XLoadQueryFont(display,fontname)) == NULL) {
        (void) fprintf(stderr,"Sorry,can't find that font.  Using 9x15 instead.\n"); 
/* If you can't find the font you want, use something standard 
 * instead so it's a polite program  */
        fontname="9x15";  
        *font_info = XLoadQueryFont(display,fontname);
   } 
}



get_GC(win,gc,font_info)
Window win;
GC *gc;
XFontStruct *font_info;
{
   unsigned long valuemask = 0;
   XGCValues values;


/* Create a default graphics context */
   *gc=XCreateGC(display,win,valuemask,&values);

/* Specify the font */
   XSetFont(display, *gc,font_info->fid); 
   XSetForeground(display, *gc, 248);        /* 248 corresponds to hot pink */
}



draw_text(win,gc,font_info)
Window win;
GC gc;
XFontStruct *font_info;
{
   char *string1 = "Another way to do things";
   char *string2 = "Press any button to quit.";
   int len1,len2;

/* need length for DrawString */
   len1=strlen(string1);
   len2=strlen(string2);

/*Output text centered in window width */
   XDrawString(display,win,gc,30,30,string1,len1);
   XDrawString(display,win,gc,30,60,string2,len2);
}
