/*
 *    draw.c:
 *
 *  First run:  This file is a sample X program designed to show how 
 *  to apply many of the techniques described in this directory.
 *
 *                 G. "Murdock" Helms, SGI Product Support -  10/31/90
 */

#include <stdio.h>               /* have to include this to know about NULL */
#include <X11/Xlib.h>            /* Xlib include files */
#include <X11/Xutil.h>           /* Must have these included */

/* Display and screen are used as arguments to nearly every Xlib
 * routine, so it simplifies things to declare them global. */

Display *display;
int screen;
Window win;

main(argc,argv)
int argc;
char **argv;
{
    Window rootwin;
    XSizeHints size_hints;
    char *window_name = "Halloween Pumpkin";
    char *icon_name = "Pumpkin";
    char *display_name = NULL;
    int x=100,y=100;
    unsigned int width=500, height=500;
    XFontStruct *font_info;         
    GC gc;        
    Colormap cmap;
    XColor orange;
    XColor green;
    XColor red;
    XEvent report;                
    unsigned long valuemask;              /* null for now */



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
                               BlackPixel(display, screen));

/* set up the colormap for usage */
    cmap=DefaultColormap(display,screen);

/* Set up all our colors for us ahead of time */
   XParseColor(display,cmap,"red",&red);
   XAllocColor(display,cmap,&red);
   XParseColor(display,cmap,"forest green",&green);
   XAllocColor(display,cmap,&green);
   XParseColor(display,cmap,"coral",&orange);
   XAllocColor(display,cmap,&orange);

/* Check that the font you want is actually there and set it
 * to something useable. */
   load_my_font(&font_info);

/* Still need a GC for fonts */
   get_GC(win,&gc,font_info);

/* Initialize size hints for window manager */
    size_hints.flags = PSize | PMinSize;
    size_hints.width = width;
    size_hints.height = height;

/* Set properties for window manager (always before mapping!)  */
    XSetStandardProperties(display, win, window_name, icon_name, None,
                           0, 0, &size_hints); 

/* Select event types wanted */
    XSelectInput(display, win, ExposureMask | ButtonPressMask);
    
/* Display window */
    XMapWindow(display, win);

/* Draw stuff */
   draw_stuff(win,gc,&green,&orange);

/* Print out text stuff */
   draw_text(win, gc, font_info,&red);

/* Get events; use the first Expose to display text and graphics;
 * ConfigureNotify to indicate a resize;
 * ButtonPress or KeyPress to exit */
    while (1) {

        XNextEvent(display, &report);

        switch (report.type) {

        /* Tells us we need to redraw */
            case Expose:
                draw_stuff(win,gc,&green,&orange);
                draw_text(win,gc,font_info,&red);
                break;

        /* The window has been resized, so change width and height
         * for draw_text and draw_graphics in next Expose */
            case ConfigureNotify:
                width = report.xconfigure.width;
                height = report.xconfigure.height;
                draw_stuff(win,gc,&green,&orange);
                draw_text(win, gc, font_info,&red);
                break;

        /* Close down gracefully */
            case ButtonPressMask:
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
   char *fontname = "9x15";


/*  Access the font  */
   if ((*font_info = XLoadQueryFont(display,fontname)) == NULL) {
        (void) fprintf(stderr,"Sorry,can't find that font.  Using fixed instead.\n"); 

/*  If it can't be found, be polite and set it to something
    else so the program doesn't just puke */
        fontname="fixed";
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
   XSetForeground(display, *gc, BlackPixel(display,screen));
}



draw_text(win,gc,font_info,red)
Window win;
GC gc;
XFontStruct *font_info;
XColor *red;
{
    char *string1 = "Press";
    char *string2="any";
    char *string3="mouse button!";
    int len1,len2,len3;


/* Need length for both XTextWidth and XDrawString */
    len1 = strlen(string1);
    len2 = strlen(string2);
    len3 = strlen(string3);

     XSetForeground(display, gc, red->pixel);
/* Output text */
    XDrawString(display, win, gc, 30, 30, string1, len1);
    XDrawString(display, win, gc, 30, 60, string2, len2);
    XDrawString(display, win, gc, 30, 90, string3, len3);
}



draw_stuff(win,gc,green,orange)
Window win;
GC gc;
XColor *green;
XColor *orange;
{
    static XPoint leye[] = {{200,270},{190,290},{210,290}};
    static XPoint reye[] = {{240,270},{230,290},{250,290}};

       
/* Let's attempt to draw a pumpkin, this being the day
 * before Halloween and all.  We'll be picking the colors by
 * hand, not by program, out of the colormap */

/* Here's a crescent moon first */
    XSetForeground(display, gc, WhitePixel(display,screen));
    XFillArc(display,win,gc,100,50,150,150,0,23040);
    XSetForeground(display, gc, BlackPixel(display,screen));
    XFillArc(display,win,gc,130,50,150,150,0,23040);

/* Then draw the ground */
    XSetForeground(display, gc, green->pixel);
    XFillRectangle(display,win,gc,0,350,500,150);

/* Here's the pumpkin body */
    XSetForeground(display, gc, orange->pixel);
    XFillArc(display,win,gc,170,250,100,100,0,23040);

/* Here's the mouth */
    XSetForeground(display, gc, BlackPixel(display,screen));
    XFillArc(display,win,gc,195,290,50,50,0,23040);
    XSetForeground(display, gc,orange->pixel);
    XFillArc(display,win,gc,195,270,50,50,0,23040);

/* Here's the left eye  */
    XSetForeground(display, gc, BlackPixel(display,screen));
    XFillPolygon(display,win,gc,leye,3,Convex,CoordModeOrigin);

/* And here's the right eye */
    XSetForeground(display, gc, BlackPixel(display,screen));
    XFillPolygon(display,win,gc,reye,3,Convex,CoordModeOrigin);
}
