/*      
 *   overlay.c:
 *
 *   This program is based on overlay_cursor.c in 4Dgifts.  It creates
 *   a house in the overlay planes and a car beneath it in the normal planes.
 *   Use the pointer to move the car out of the house.
 *
 *   Unlike overlay_cursor.c, it does not mofidy the cursor.
 *
 *   If there are no overlay planes, it will use popup planes.
 */

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/keysym.h>
#include <Xm/Frame.h>
#include <X11/Xirisw/GlxMDraw.h>

int   oldx, oldy, curx, cury, dx, dy;

/* The GLX configuration parameter:
 * 	Double buffering
 *	color index (default so unspecified)
 *	overlay
 *	nothing else special
 */
static GLXconfig glxConfig [] = {
    { GLX_NORMAL, GLX_DOUBLE, TRUE },
    { GLX_OVERLAY, GLX_BUFSIZE, 2},
    { 0, 0, 0 }
};

String fallback_resources[] = {
    "*geometry: =750x600",
    "*frame*shadowType: SHADOW_IN",
    NULL
};

Boolean use_pups=FALSE;	/* if TRUE, must use popup planes */

static XtAppContext app_context;

/* forward declarations of callbacks */
static void exposeCB();
static void overlayExposeCB();
static void resizeCB();
static void initCB();
static void inputCB();

main(argc, argv)
int argc;
char *argv[];
{
    Arg args[20];
    int n = 0;
    Widget toplevel, frame, glw;

    toplevel = XtAppInitialize(&app_context, "4DgiftsGlx", 
			       (XrmOptionDescList)NULL , 0,
			       (Cardinal*)&argc, 
			       (String*)argv, 
			       fallback_resources,
			       (ArgList)NULL, 0);
    /* check if we must use pups */
    if (getgdesc(GD_BITS_OVER_SNG_CMODE) < 2)
	use_pups = TRUE;
    n = 0;
    frame = XtCreateManagedWidget("frame",
				  xmFrameWidgetClass,
				  toplevel, args, n);
    /* if using pups, modify the config parameter */
    if (use_pups)
	glxConfig[1].buffer = GLX_POPUP;
    n = 0;
    XtSetArg(args[n], GlxNglxConfig, glxConfig); n++;
    XtSetArg(args[n], use_pups?GlxNusePopup:GlxNuseOverlay, TRUE); n++;
    glw = XtCreateManagedWidget("glwidget",
				  glxMDrawWidgetClass,
				  frame, args, n);

    XtAddCallback(glw, GlxNexposeCallback, exposeCB, 0);
    XtAddCallback(glw,
		  use_pups?GlxNpopupExposeCallback:GlxNoverlayExposeCallback,
		  overlayExposeCB, 0);
    XtAddCallback(glw, GlxNresizeCallback, resizeCB, 0);
    XtAddCallback(glw, GlxNginitCallback, initCB, 0);
    XtAddCallback(glw, GlxNinputCallback, inputCB, 0);

    XtRealizeWidget(toplevel);
    installColormapWithOverlay(toplevel,glw);


    XtAppMainLoop(app_context);

}

/* Return the overlay window of the widget */
Window overlayWindow(w)
Widget w;
{
    Arg args[1];
    Window overlayWindow;

    XtSetArg(args[0], use_pups?GlxNpopupWindow:GlxNoverlayWindow,
	     &overlayWindow);
    XtGetValues(w, args, 1);
    return (overlayWindow);
}

/* Callbacks */
static void
exposeCB(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    GlxDrawCallbackStruct *call_data;
{
    GLXwinset(XtDisplay(w), call_data->window);
    drawscene (dx, dy);
    swapbuffers ();
}

static void
overlayExposeCB(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    GlxDrawCallbackStruct *call_data;
{
    GLXwinset(XtDisplay(w), call_data->window);
    drawhouse();
}

static void
resizeCB(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    GlxDrawCallbackStruct *call_data;
{
    GLXwinset(XtDisplay(w), call_data->window);
    viewport(0, (Screencoord) call_data->width-1,
	     0, (Screencoord) call_data->height-1);
    drawscene(dx,dy);
    GLXwinset(XtDisplay(w), overlayWindow(w));
    viewport(0, (Screencoord) call_data->width-1,
	     0, (Screencoord) call_data->height-1);
    drawhouse();
}

static void
initCB(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    GlxDrawCallbackStruct *call_data;
{
    GLXwinset(XtDisplay(w), call_data->window);
    initialize_gl();
    dx = call_data->width / 2;
    dy = call_data->height / 2;
    GLXwinset(XtDisplay(w), overlayWindow(w));
    initialize_overlay();
}

static void
inputCB(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    GlxDrawCallbackStruct *call_data;
{
    char buffer[1];
    KeySym keysym;
    static Position oldx, oldy, newx, newy;
    static Boolean buttonDown = FALSE;

    GLXwinset(XtDisplay(w), call_data->window);
    switch(call_data->event->type)
    {
    case KeyRelease:
	/* It is necessary to convert the keycode to a keysym before
	 * it is possible to check if it is an escape
	 */
	if (XLookupString(call_data->event,buffer,1,&keysym,NULL) == 1 &&
	    keysym == (KeySym)XK_Escape)
	    exit(0);
	break;
    case ButtonPress:
	switch(call_data->event->xbutton.button)
	{
	case Button1:
	    buttonDown = TRUE;
	    oldx = newx = call_data->event->xbutton.x;
	    oldy = newy = call_data->event->xbutton.y;
	    break;
	}
	break;
    case ButtonRelease:
	switch(call_data->event->xbutton.button)
	{
	case Button1:
	    buttonDown = FALSE;;
	    break;
	}
	break;
    case MotionNotify:
	if (buttonDown && call_data->event->xmotion.state & Button1Mask)
	{
	    oldx = newx;
	    oldy = newy;
	    newx = call_data->event->xbutton.x;
	    newy = call_data->event->xbutton.y;
	    dx += newx-oldx;
	    /* GL and X have opposite y coordinates so subtract from dy */
	    dy -= newy-oldy;
	    drawscene (dx, dy);
	    swapbuffers ();
	}
	break;
    }
}

initialize_gl () {
    long xmaxscrn, ymaxscrn;     /* maximum size of screen in x and y       */
    long xscrnsize;              /* size of screen in x used to set globals */
    Colorindex dummy;            /* for strict ANSI C prototyping           */

    xscrnsize = getgdesc(GD_XPMAX);       /* get/set screen size[/aspect] */
    if (xscrnsize == 1280) {
         xmaxscrn = 1279;
         ymaxscrn = 1023;
         keepaspect(5, 4);
    } else if (xscrnsize == 1024) {
         xmaxscrn = 1023;
         ymaxscrn = 767;
         keepaspect(4, 3);
    } else {
        fprintf(stderr, "Something's EXTREMELY wrong:  ");
        fprintf(stderr, "xscrnsize=%d\n", xscrnsize);
        exit(-1) ;
    }
    doublebuffer ();
    shademodel (FLAT);
}

initialize_overlay()
{
    mapcolor (1, 255, 0, 255);
    mapcolor (2, 0, 255, 255);
}

/*  Everytime through the loop, draw only the car, not the house.  */
drawscene (x, y)
int     x, y;
{
    color (BLACK);
    clear ();
    drawcar (x, y);
    gflush();
}

/*  draw a car with several colors.
 *  The car itself is drawn with  the front window first.  Then the front
 *  window is flipped over (scaled) for the  rear  window.  The translate
 *  routine moves the car to an (x,y) position.
 */
drawcar (x, y)
int     x, y;
{
    float   fx, fy;

    fx = (float) x;
    fy = (float) y;
    pushmatrix ();
    translate (fx, fy, 0.0);        /*  move to mouse location  */
    color (BLUE);                   /*  wheels  */
    circfi (-75, -75, 20);
    circfi (75, -75, 20);
    color (RED);                    /*  car body  */
    pmv2i (-150, -50);
    pdr2i (-125, 0);
    pdr2i (125, 0);
    pdr2i (150, -50);
    pclos ();
    color (YELLOW);                 /*  front window  */
    drawwindow ();
    color (GREEN);                  /*  rear window  */
    scale (-1.0, 1.0, 1.0);
    drawwindow ();
    popmatrix ();
}

/*  draw a window for the car  */
drawwindow () {
    pmv2i (0, 0);
    pdr2i (0, 50);
    pdr2i (50, 50);
    pdr2i (75, 0);
    pclos ();
}

/*  draw a house in two colors at a fixed position, specified by the trans-
 *  late routine.  The house is drawn with colors in the 4D overlay bitplanes.
 */
drawhouse () {
    color (0);
    clear ();
    pushmatrix ();
    translate (200.0, 100.0, 0.0);     /*  move house into position  */
    color (1);                         /*  roof   */
    pmv2i (0, 0);
    pdr2i (0, 250);
    pdr2i (350, 250);
    pdr2i (350, 0);
    pclos ();
    color (2);                         /*  1st floor  */
    pmv2i (175, 400);
    pdr2i (0, 250);
    pdr2i (350, 250);
    pclos ();
    popmatrix ();
    gflush();
}

