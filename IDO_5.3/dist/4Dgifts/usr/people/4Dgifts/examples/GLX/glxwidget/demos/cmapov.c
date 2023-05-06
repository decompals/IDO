/** header ******************************************************************/

/*
// $RCSfile: cmapov.c,v $
// $Revision: 1.1 $
// $Date: 1993/02/16 13:54:16 $
// $Author: martinm $
//
// creator: Brett Bainter
//
// purpose:
//	mixed model program demonstrating
//	...using custom colormaps for the normal and overlay buffers.
//	...moving things with the mouse.
//
// compiling:
//	cc -float -prototypes -DFUNCPROTO -O cmapov.c -o cmapov \
//	-s -lXirisw -lXm_s -lXt_s -lgl_s -lX11_s -lm -lsun -lPW
//
// operating:
//	Use the left mouse button to move the red, green, and blue blocks.
//	Verify that they "pass under" the yellow, magenta, and cyan blocks
//	which are in the overlay planes.
//
*/

/** notes *******************************************************************/

/*
// bugs:
//	There is a known bug with 8 bit PI's that will cause this program to
//	run in false colors.  It will initially come up correctly but when
//	the cursor moves to a gl window (for example, showmap), the colormap
//	will not be reloaded when the cursor returns to the cmapov window.
*/

/** includes ****************************************************************/

#include <stdio.h>			/* standard */
#include <Xm/Xm.h>			/* for motif */
#include <Xm/Form.h>			/* motif widget */
#include <Xm/Frame.h>			/* motif widget */
#include <Xm/PushB.h>			/* motif widget */
#include <Xm/RowColumn.h>		/* motif widget */
#include <Xm/Separator.h>		/* motif widget */
#include <X11/Xirisw/GlxMDraw.h>	/* gl widget */

/** defines *****************************************************************/

/* c environment */
#define global

/** typedefs ****************************************************************/
/** prototypes **************************************************************/

extern void main(int argc, char *argv[], char *envp[]);

/* setup */
static void  install_colormaps(Widget top_level, Widget glw);
static void  normal_cmap_init(Widget glw);
static Pixel normal_cmap_set(Widget glw, int index, short r, short g, short b);
static void  overlay_cmap_init(Widget glw);
static Pixel overlay_cmap_set(Widget glw, int index, short r, short g, short b);

/* mixed model support */
static void gl_ginit_cb(Widget w, XtPointer appdat, XtPointer sysdat);
static void gl_expose_cb(Widget w, XtPointer appdat, XtPointer sysdat);
static void gl_resize_cb(Widget w, XtPointer appdat, XtPointer sysdat);
static void gl_input_cb(Widget w, XtPointer appdat, XtPointer sysdat);
static void gl_overlay_expose_cb(Widget w, XtPointer appdat, XtPointer sysdat);

/* callbacks (misc) */
static void quit_cb(Widget w, XtPointer appdat, XtPointer sysdat);

/* drawing */
static void draw_normal_frame(void);
static void draw_overlay_frame(void);
static void draw_boxes(int c1, int c2, int c3);

/** variables ***************************************************************/

/* mixed-model configuration */
static GLXconfig glx_config[] = {
    {GLX_NORMAL, GLX_DOUBLE, TRUE},
    {GLX_OVERLAY, GLX_BUFSIZE, 2},
    { 0, 0, 0 },
};

/* information which allows us to use the overlay or popup buffer */
static struct {
    char *use;
    char *expose_cb;
    char *window;
    char *visual;
    char *colormap;
} *over_res, over_res_map[2] = {
    /* describe needed overlay resources */
    {   GlxNuseOverlay, GlxNoverlayExposeCallback, GlxNoverlayWindow,
	GlxNoverlayVisual, GlxNoverlayColormap
    },
    /* describe analogous popup resources for when overlays aren't there */
    {   GlxNusePopup, GlxNpopupExposeCallback, GlxNpopupWindow,
	GlxNpopupVisual, GlxNpopupColormap
    },
};

/* normal buffer colors */
static Pixel n_grey, n_red, n_green, n_blue;

/* overlay buffer colors */
static Pixel o_trans, o_yellow, o_magenta, o_cyan;

/* gl window info */
static struct {
    Dimension width;		/* in pixels */
    Dimension height;		/* in pixels */
    float pt[3];		/* world position of moving object */
} glwin = {400, 400, {15.0, 20.0, 0.0}};

/** functions ***************************************************************/

/*
// main - program entry point.
*/
global void main(
    int argc,			/* argument count */
    char *argv[],		/* argument vector */
    char *envp[]		/* environment pointer */
)
{
    XtAppContext app_context;	/* application context */
    Widget app_shell;		/* first widget */
    Widget form;		/* surrounds app */
    Widget rowcol;		/* manages input buttons */
    Widget button;		/* quit button */
    Widget separator;		/* between input and output */
    Widget frame;		/* to surround gl widget */
    Widget glw;			/* the gl widget inside window */
    Arg args[15];		/* for name/value pairs */
    int n;			/* for reusable indices */

    /* perform capabilities check */
    /* use popup planes if there is not enough overlay planes */
    over_res = &over_res_map[0];
    if (getgdesc(GD_BITS_OVER_SNG_CMODE) < 2) {
	glx_config[1].buffer = GLX_POPUP;
	over_res = &over_res_map[1];
    }
    printf(
	"\nUsing the %s planes\n", over_res==over_res_map? "OVERLAY" : "POPUP"
    );

    /* initialize toolkit, creating application shell */
    n = 0;
    XtSetArg(args[n], XmNtitle, "CMode Overlay"); n++;
    app_shell = XtAppInitialize(
	&app_context, "Cmapov", NULL, 0, &argc, argv, NULL,
	args, n
    );

    /* create container for app */
    n = 0;
    form = XmCreateForm(app_shell, "form", args, n);
    XtManageChild(form);

    /* create the command area */
    n = 0;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
    rowcol = XmCreateRowColumn(form, "rowcol", args, n);
    XtManageChild(rowcol);

    /* create the command area buttons */
    n = 0;
    button = XmCreatePushButton(rowcol, "Quit", args, n);
    XtAddCallback(button, XmNactivateCallback, quit_cb, NULL);
    XtManageChild(button);

    /* create separator between command area and output area */
    n = 0;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNleftWidget, rowcol); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNorientation, XmVERTICAL); n++;
    separator = XmCreateSeparator(form, "separator", args, n);
    XtManageChild(separator);

    /* create the output area */
    /* create the frame */
    n = 0;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNleftWidget, separator); n++;
    XtSetArg(args[n], XmNleftOffset, 5); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightOffset, 5); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNbottomOffset, 5); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNtopOffset, 5); n++;
    XtSetArg(args[n], XmNshadowThickness, 6); n++;
    frame = XmCreateFrame(form, "frame", args, n);
    XtManageChild(frame);

    /* create the gl widget */
    n = 0;
    XtSetArg(args[n], GlxNglxConfig, glx_config); n++;
    XtSetArg(args[n], over_res->use, True); n++;
    XtSetArg(args[n], XmNborderWidth, 0); n++;
    XtSetArg(args[n], XmNwidth, glwin.width); n++;
    XtSetArg(args[n], XmNheight, glwin.height); n++;
    glw = GlxCreateMDraw(frame, "glw", args, n);
    XtManageChild(glw);
    XtAddCallback(glw, GlxNginitCallback, gl_ginit_cb, 0);
    XtAddCallback(glw, GlxNexposeCallback, gl_expose_cb, 0);
    XtAddCallback(glw, GlxNresizeCallback, gl_resize_cb, 0);
    XtAddCallback(glw, GlxNinputCallback, gl_input_cb, 0);
    XtAddCallback(glw, over_res->expose_cb, gl_overlay_expose_cb, 0);

    /* setup custom normal colormap */
    normal_cmap_init(glw);
    n_grey  = normal_cmap_set(glw, 0, 125, 125, 125);
    n_red   = normal_cmap_set(glw, 1, 255,   0,   0);
    n_green = normal_cmap_set(glw, 2,   0, 255,   0);
    n_blue  = normal_cmap_set(glw, 3,   0,   0, 255);

    /* setup custom overlay colormap */
    overlay_cmap_init(glw);
    o_trans   = 0;	/* transparent is always zero */
    o_yellow  = overlay_cmap_set(glw, 0, 255, 255,   0);
    o_magenta = overlay_cmap_set(glw, 1, 255,   0, 255);
    o_cyan    = overlay_cmap_set(glw, 2,   0, 255, 255);

    /* realize the app, creating the actual x windows */
    XtRealizeWidget(app_shell);

    /* setup for colormap installation */
    install_colormaps(app_shell, glw);

    /* enter the event loop */
    XtAppMainLoop(app_context);
}


/*- support: setup ---------------------------------------------------------*/
/*
// install_colormaps - let the window manager know about our colormaps.
//
// This has been generalized to handle any windows a gl widget might have.
// It may not necessarily being using any of them.
*/
static void install_colormaps(Widget top_level, Widget glw)
{
    Window overlay_win, popup_win, underlay_win;
    Window window[5];
    int i;

    XtVaGetValues(
	glw,
	GlxNoverlayWindow, &overlay_win,
	GlxNpopupWindow, &popup_win,
	GlxNunderlayWindow, &underlay_win,
	NULL
    );
    i = 0;
    if (overlay_win)
	window[i++] = overlay_win;
    if (popup_win)
	window[i++] = popup_win;
    if (underlay_win)
	window[i++] = underlay_win;
    window[i++] = XtWindow(glw);
    window[i++] = XtWindow(top_level);
    XSetWMColormapWindows(XtDisplay(top_level), XtWindow(top_level), window, i);
}


/*- support: custom normal colormap ----------------------------------------*/
/*
// normal_cmap_init - create a new normal colormap for the gl widget.
//
// The gl widget must already be created prior to calling this function,
// however the gl widget does not need to be realized for it to work.  This
// is because the window it uses in creating the colormap is the root window
// on the same screen.
*/
static void normal_cmap_init(Widget glw)
{
    Display *display;
    Window window;
    XVisualInfo *visinfo;
    Colormap pmap;
    Colormap cmap;
    XColor *color;
    int ncolors;
    int i;

    /* get display; any window on the same screen; and the visual */
    display = XtDisplay(glw);
    window = RootWindowOfScreen(XtScreen(glw));
    XtVaGetValues(glw, XmNvisual, &visinfo, NULL);

    /* create new normal colormap, allocating all entries */
    cmap = XCreateColormap(display, window, visinfo->visual, AllocAll);

    /* set new normal colormap for the gl widget */
    XtVaSetValues(glw, XmNcolormap, cmap, NULL);

    /*
    // duplicate the parent's default colors for the lower colormap entries
    // (max 256) to avoid colormap flashing on machines with only one h/w
    // colormap.
    */
    XtVaGetValues(XtParent(glw), XmNcolormap, &pmap, NULL);
    ncolors = visinfo->colormap_size;
    printf("\nnormal colors = %d\n", ncolors);
    if (ncolors > 256)
	ncolors = 256;
    color = (XColor *) XtMalloc(ncolors*sizeof(XColor));
    for (i=0; i<ncolors; i++)
	color[i].pixel = i;
    XQueryColors(display, pmap, color, ncolors);
    XStoreColors(display, cmap, color, ncolors);
    XtFree((char *)color);
}


/*
// normal_cmap_set - map a color for the normal buffer.
//
// This uses a simple scheme of mapping the colors backwards from the highest
// colormap index.
*/
static Pixel normal_cmap_set(Widget glw, int index, short r, short g, short b)
{
    XVisualInfo *visinfo;
    Colormap cmap;
    XColor color;
    int n_last;

    XtVaGetValues(glw, XmNvisual, &visinfo, XmNcolormap, &cmap, NULL);
    n_last = visinfo->colormap_size-1;
    color.pixel = n_last - index; /* work backwards from the last position */
    color.flags = DoRed | DoGreen | DoBlue;
    color.red   = r << 8;
    color.green = g << 8;
    color.blue  = b << 8;
    XStoreColor(XtDisplay(glw), cmap, &color);
    return (color.pixel);
}


/*- support: custom overlay colormap ---------------------------------------*/
/*
// overlay_cmap_init - create a new overlay colormap for the gl widget.
//
// The gl widget must already be created prior to calling this function,
// however the gl widget does not need to be realized for it to work.  This
// is because the window it uses in creating the colormap is the root window
// on the same screen.
*/
static void overlay_cmap_init(Widget glw)
{
    Display *display;
    Window window;
    XVisualInfo *visinfo;
    Colormap cmap;
    XColor color;
    int ncolors;
    Pixel *pixel;
    unsigned long plane_mask[1];
    int result;

    /* get display; any window on the same screen; and the visual */
    display = XtDisplay(glw);
    window = RootWindowOfScreen(XtScreen(glw));
    XtVaGetValues(glw, over_res->visual, &visinfo, NULL);

    /*
     * create new overlay colormap, allocating no entries.
     * (AllocAll would fail here because index 0 is reserved for transparency)
     */
    cmap = XCreateColormap(display, window, visinfo->visual, AllocNone);

    /* set new overlay colormap for the gl widget */
    XtVaSetValues(glw, over_res->colormap, cmap, NULL);

    /* allocate every color except transparency as read/write */
    ncolors = visinfo->colormap_size;	/* including transparent color */
    printf("\noverlay colors = %d\n", ncolors);
    pixel = (Pixel *) XtMalloc(ncolors*sizeof(Pixel));	/* stub array */
    result = XAllocColorCells(
	display, cmap, True, plane_mask, 0,
	&pixel[1], ncolors-1	/* one less due to transparency */
    );
    XtFree((char *) pixel);

    /* check for booboo */
    if (result == 0)
	fprintf(stderr, "XAllocColorCells failed for overlay buffer.\n");
}


/*
// overlay_cmap_set - map a color for the overlay buffer.
//
// This uses a simple scheme of mapping the colors backwards from the highest
// colormap index.
*/
static Pixel overlay_cmap_set(Widget glw, int index, short r, short g, short b)
{
    XVisualInfo *visinfo;
    Colormap cmap;
    XColor color;
    int n_last;

    XtVaGetValues(
	glw, over_res->visual, &visinfo, over_res->colormap, &cmap, NULL
    );
    n_last = visinfo->colormap_size-1;
    color.pixel = n_last - index; /* work backwards from the last position */
    color.flags = DoRed | DoGreen | DoBlue;
    color.red   = r << 8;
    color.green = g << 8;
    color.blue  = b << 8;
    XStoreColor(XtDisplay(glw), cmap, &color);
    return (color.pixel);
}


/*- support: callbacks (gl widget) -----------------------------------------*/
/*
// gl_ginit_cb - perform any necessary graphics initialization.
*/
static void gl_ginit_cb(Widget w, XtPointer appdat, XtPointer sysdat)
{
    GlxDrawCallbackStruct *glx = (GlxDrawCallbackStruct *) sysdat;

    GLXwinset(XtDisplay(w), XtWindow(w));
    mmode(MVIEWING);
    ortho2(-0.5, 100.5, -0.5, 100.5);
    gflush();
}


/*
// gl_expose_cb - handle expose events for the gl widget.
*/
static void gl_expose_cb(Widget w, XtPointer appdat, XtPointer sysdat)
{
    GlxDrawCallbackStruct *glx = (GlxDrawCallbackStruct *) sysdat;

    GLXwinset(XtDisplay(w), XtWindow(w));
    draw_normal_frame();
}


/*
// gl_resize_cb - handle resize events for the gl widget.
*/
static void gl_resize_cb(Widget w, XtPointer appdat, XtPointer sysdat)
{
    GlxDrawCallbackStruct *glx = (GlxDrawCallbackStruct *) sysdat;
    Window overlay_window;

    /* squirrel away size */
    glwin.width = glx->width;
    glwin.height = glx->height;

    /* setup normal buffer viewport */
    GLXwinset(XtDisplay(w), XtWindow(w));
    viewport(0, glx->width-1, 0, glx->height-1);

    /* setup overlay buffer viewport */
    XtVaGetValues(w, over_res->window, &overlay_window, NULL);
    GLXwinset(XtDisplay(w), overlay_window);
    viewport(0, glx->width-1, 0, glx->height-1);
}


/*
// gl_input_cb - handle input for the gl window.
*/
static void gl_input_cb(Widget w, XtPointer appdat, XtPointer sysdat)
{
    static Boolean active = False;	/* currently moving? */
    static float dx, dy;		/* offset from current position */
    /**/
    GlxDrawCallbackStruct *glx = (GlxDrawCallbackStruct *) sysdat;
    XEvent *event = glx->event;	/* what occured */
    int msx, msy;		/* gl window mouse position */
    float mwx, mwy;		/* gl world  mouse position */

    GLXwinset(XtDisplay(w), XtWindow(w));

    /* map to gl window coords */
    msx = event->xbutton.x;			/* same x */
    msy = (glwin.height-1) - event->xbutton.y;	/* flip y */

    /* map to gl world coords */
    mwx = 0.0 + ((msx - 0) / (float)glwin.width ) * 100.0;
    mwy = 0.0 + ((msy - 0) / (float)glwin.height) * 100.0;

    /* process event */
    switch (event->type) {
    case ButtonPress:
	if (event->xbutton.button == Button1) {
	    /* compute delta from current position */
	    dx = mwx - glwin.pt[0];
	    dy = mwy - glwin.pt[1];
	    active = True;
	}
	break;
    case MotionNotify:
	if (active) {
	    /* compute new position and draw */
	    glwin.pt[0] = mwx - dx;
	    glwin.pt[1] = mwy - dy;
	    draw_normal_frame();
	}
	break;
    case ButtonRelease:
	if (event->xbutton.button == Button1) {
	    /* we're done */
	    active = False;
	}
	break;
    }
}


/*
// gl_overlay_expose_cb - handle overlay expose events for the gl widget.
*/
static void gl_overlay_expose_cb(Widget w, XtPointer appdat, XtPointer sysdat)
{
    GlxDrawCallbackStruct *glx = (GlxDrawCallbackStruct *) sysdat;

    GLXwinset(XtDisplay(w), glx->window);
    draw_overlay_frame();
}


/*- support: callbacks (misc) ----------------------------------------------*/
/*
// quit_cb - exit application.
*/
static void quit_cb(Widget w, XtPointer appdat, XtPointer sysdat)
{
    exit(0);
}


/*- support: drawing -------------------------------------------------------*/
/* 
// draw_normal_frame - render objects in the normal buffer and swap.
*/
static void draw_normal_frame(void)
{
    color(n_grey);
    clear();
    pushmatrix();
	translate(glwin.pt[0], glwin.pt[1], glwin.pt[2]);
	draw_boxes(n_red, n_green, n_blue);
    popmatrix();
    swapbuffers();
    gflush();
}


/* 
// draw_overlay_frame - render objects in the overlay buffer.
*/
static void draw_overlay_frame(void)
{
    color(o_trans);
    clear();
    pushmatrix();
	translate(15.0, 60.0, 0.0);
	draw_boxes(o_yellow, o_cyan, o_magenta);
    popmatrix();
    gflush();
}


/*
// draw_boxes - draw three boxes in three different colors.
*/
static void draw_boxes(int c1, int c2, int c3)
{
    static float vert[][2] = {	/* a box */
	{ 0.0,  0.0},
	{20.0,  0.0},
	{20.0, 20.0},
	{ 0.0, 20.0},
    };

    pushmatrix();
    color(c1);
    bgnpolygon();
	v2f(vert[0]); v2f(vert[1]); v2f(vert[2]); v2f(vert[3]);
    endpolygon();
    translate(25.0, 0.0, 0.0);
    color(c2);
    bgnpolygon();
	v2f(vert[0]); v2f(vert[1]); v2f(vert[2]); v2f(vert[3]);
    endpolygon();
    translate(25.0, 0.0, 0.0);
    color(c3);
    bgnpolygon();
	v2f(vert[0]); v2f(vert[1]); v2f(vert[2]); v2f(vert[3]);
    endpolygon();
    popmatrix();
}

/** eof *********************************************************************/
