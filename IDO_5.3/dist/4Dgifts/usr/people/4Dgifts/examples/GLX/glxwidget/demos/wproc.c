/** header ******************************************************************/

/*
// $RCSfile: wproc.c,v $
// $Revision: 1.1 $
// $Date: 1993/02/16 13:54:33 $
// $Author: martinm $
//
// creator: Brett Bainter
//
// purpose:
//	mixed model motif program demonstrating
//	... using workprocs for continuous animation
//	... updating aspect of 3d view to keep "square's square"
//	... creating popup menu for the gl widget
//
// compiling:
//	cc -float -prototypes -DFUNCPROTO -O wproc.c -o wproc \
//	-s -lXirisw -lXm_s -lXt_s -lgl_s -lX11_s -lm -lsun -lPW
*/

/** notes *******************************************************************/
/** includes ****************************************************************/

#include <stdio.h>			/* printf(), ... */
#include <Xm/Xm.h>			/* for motif */
#include <Xm/Form.h>			/* motif widget */
#include <Xm/Frame.h>			/* motif widget */
#include <Xm/Label.h>			/* motif widget */
#include <Xm/PushB.h>			/* motif widget */
#include <Xm/RowColumn.h>		/* motif widget */
#include <Xm/Separator.h>		/* motif widget */
#include <X11/Xirisw/GlxMDraw.h>	/* gl widget */

/** defines *****************************************************************/

/* c environment */
#define global

/* colors */
#define RGB_BLACK	0x00000000
#define RGB_RED		0x000000FF
#define RGB_GREEN	0x0000FF00
#define RGB_BLUE	0x00FF0000

/** typedefs ****************************************************************/
/** prototypes **************************************************************/

extern void main(int argc, char *argv[], char *envp[]);

/* setup */
static void check_capabilities(void);
static void install_colormaps(Widget top_level, Widget glw);

/* callbacks (gl widget) */
static void gl_ginit_cb(Widget w, XtPointer appdat, XtPointer sysdat);
static void gl_expose_cb(Widget w, XtPointer appdat, XtPointer sysdat);
static void gl_resize_cb(Widget w, XtPointer appdat, XtPointer sysdat);
static void gl_input_cb(Widget w, XtPointer appdat, XtPointer sysdat);

/* callbacks (misc) */
static void quit_cb(Widget w, XtPointer appdat, XtPointer sysdat);
static void color_cb(Widget w, XtPointer appdat, XtPointer sysdat);

/* event handlers */
static void post_menu_eh(Widget w, Widget menu, XEvent *event);

/* work procedures */
static Boolean anim_wp(XtPointer appdat);

/* drawing */
static void draw_frame(char *ops);
static void model_cube_wire(void);

/** variables ***************************************************************/

/* fallback resources */
static char *fallback_resources[] = {
    "Wproc*Red*foreground: red",
    "Wproc*Green*foreground: green4",
    "Wproc*Blue*foreground: blue",
    "Wproc*info_label*labelString: "
	"[ Use the Right Mouse Button to pop up color menu ]",
    NULL,
};

/*
// mixed-model configuration:
*/
static GLXconfig glx_config[] = {
    {GLX_NORMAL, GLX_DOUBLE, TRUE},
    {GLX_NORMAL, GLX_RGB, TRUE},
    { 0, 0, 0 },
};

static unsigned long cube_color = RGB_GREEN;

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
    Display *dsp;		/* display ref */
    Widget app_shell;		/* first widget */
    Widget form;		/* surrounds app */
    Widget rowcol;		/* manages input buttons */
    Widget button;		/* utility button */
    Widget label;		/* utility label */
    Widget separator;		/* utility separator */
    Widget frame;		/* to surround gl widget */
    Widget glw;			/* can do gl rendering in this guy */
    Widget menu;		/* simple popup for gl widget */
    XtWorkProcId anim_wpid;	/* animation work proc */
    Arg args[15];		/* for name/value pairs */
    int n;			/* reusable indices */

    /* make sure we can we do it */
    check_capabilities();

    /* initialize toolkit, creating application shell */
    n = 0;
    XtSetArg(args[n], XmNtitle, "Work Proc"); n++;
    app_shell = XtAppInitialize(
	&app_context, "Wproc", NULL, 0, &argc, argv,
	fallback_resources, args, n
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
    /* create the informational label */
    n = 0;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNleftWidget, separator); n++;
    XtSetArg(args[n], XmNleftOffset, 5); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightOffset, 5); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNtopOffset, 5); n++;
    label = XmCreateLabel(form, "info_label", args, n);
    XtManageChild(label);

    /* create the frame */
    n = 0;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNleftWidget, separator); n++;
    XtSetArg(args[n], XmNleftOffset, 5); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightOffset, 5); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNbottomOffset, 5); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget, label); n++;
    XtSetArg(args[n], XmNtopOffset, 5); n++;
    XtSetArg(args[n], XmNshadowThickness, 6); n++;
    frame = XmCreateFrame(form, "frame", args, n);
    XtManageChild(frame);

    /* create the gl widget */
    n = 0;
    XtSetArg(args[n], GlxNglxConfig, glx_config); n++;
    XtSetArg(args[n], XmNborderWidth, 0); n++;
    XtSetArg(args[n], XmNwidth, 400); n++;
    XtSetArg(args[n], XmNheight, 400); n++;
    glw = GlxCreateMDraw(frame, "glw", args, n);
    XtManageChild(glw);
    XtAddCallback(glw, GlxNginitCallback, gl_ginit_cb, 0);
    XtAddCallback(glw, GlxNexposeCallback, gl_expose_cb, 0);
    XtAddCallback(glw, GlxNresizeCallback, gl_resize_cb, 0);
    XtAddCallback(glw, GlxNinputCallback, gl_input_cb, 0);

    /* create a popup menu */
    n = 0;
    menu = XmCreatePopupMenu(form, "menu", args, n);
    XtAddEventHandler(
	form, ButtonPressMask, FALSE, (XtEventHandler) post_menu_eh,
	(XtPointer) menu
    );
    /* menu title is the name of the program */
    n = 0;
    label = XmCreateLabel(menu, "Color", args, n);
    XtManageChild(label);
    separator = XmCreateSeparator(menu, "separator", args, n);
    XtManageChild(separator);
    separator = XmCreateSeparator(menu, "separator", args, n);
    XtManageChild(separator);
    /* add some buttons to change color */
    n = 0;
    button = XmCreatePushButton(menu, "Red", args, n);
    XtAddCallback(button, XmNactivateCallback, color_cb, (XtPointer) RGB_RED);
    XtManageChild(button);
    button = XmCreatePushButton(menu, "Green", args, n);
    XtAddCallback(button, XmNactivateCallback, color_cb, (XtPointer) RGB_GREEN);
    XtManageChild(button);
    button = XmCreatePushButton(menu, "Blue", args, n);
    XtAddCallback(button, XmNactivateCallback, color_cb, (XtPointer) RGB_BLUE);
    XtManageChild(button);

    /* setup work procedure */
    anim_wpid = XtAppAddWorkProc(app_context, anim_wp, (XtPointer) glw);

    /* realize the app, creating the actual x windows */
    XtRealizeWidget(app_shell);
    install_colormaps(app_shell, glw);

    /* enter the event loop */
    XtAppMainLoop(app_context);
}


/*- support: setup ---------------------------------------------------------*/
/*
// check_capabilities - find out if the machine can do what we need.
*/
static void check_capabilities(void)
{
    if (getgdesc(GD_BITS_NORM_DBL_RED) == 0) {
	fprintf(stderr, "Double buffered RGB mode not available.\n");
	exit(1);
    }
}


/*
// install_colormaps - let the window manager know about our colormaps.
//
// this should be done even for rgb mode apps because some sgi systems
// implement rgb mode via colormaps.
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


/*- support: callbacks (gl widget) -----------------------------------------*/
/*
// gl_ginit_cb - perform any necessary graphics initialization.
*/
static void gl_ginit_cb(Widget w, XtPointer appdat, XtPointer sysdat)
{
    GlxDrawCallbackStruct *glx = (GlxDrawCallbackStruct *) sysdat;

    GLXwinset(XtDisplay(w), XtWindow(w));

    mmode(MVIEWING);
    perspective(300, glx->width/(float)glx->height, 1.0, 50.0);
    polarview(10.0, 0, 0, 0);
    frontbuffer(TRUE);
    cpack(RGB_BLACK);
    clear();
    frontbuffer(FALSE);
    gflush();
}


/*
// gl_expose_cb - handle expose events for the gl widget.
*/
static void gl_expose_cb(Widget w, XtPointer appdat, XtPointer sysdat)
{
    GlxDrawCallbackStruct *glx = (GlxDrawCallbackStruct *) sysdat;

    GLXwinset(XtDisplay(w), XtWindow(w));
    draw_frame("cds");
}


/*
// gl_resize_cb - handle resize events for the gl widget.
*/
static void gl_resize_cb(Widget w, XtPointer appdat, XtPointer sysdat)
{
    GlxDrawCallbackStruct *glx = (GlxDrawCallbackStruct *) sysdat;

    GLXwinset(XtDisplay(w), XtWindow(w));
    viewport(0, glx->width-1, 0, glx->height-1);
    perspective(300, glx->width/(float)glx->height, 1.0, 50.0);
}


/*
// gl_input_cb - handle input from a gl window.
*/
static void gl_input_cb(Widget w, XtPointer appdat, XtPointer sysdat)
{
    GlxDrawCallbackStruct *glx = (GlxDrawCallbackStruct *) sysdat;

    GLXwinset(XtDisplay(w), XtWindow(w));
}


/*- support: callbacks (misc) ----------------------------------------------*/
/*
// quit_cb -
*/
static void quit_cb(Widget w, XtPointer appdat, XtPointer sysdat)
{
    exit(0);
}


/*
// color_cb -
*/
static void color_cb(Widget w, XtPointer appdat, XtPointer sysdat)
{
    cube_color = (unsigned long) appdat;
}


/*- support: event handlers ------------------------------------------------*/
/*
// post_menu_eh -
*/
static void post_menu_eh(Widget w, Widget menu, XEvent *event)
{
    int button;

    /* make sure it's the correct button being pressed */
    XtVaGetValues(menu, XmNwhichButton, &button, NULL);
    if (event->xbutton.button == button) { 
	XmMenuPosition(menu, (XButtonPressedEvent *) event);
	XtManageChild(menu);
    }
}


/*- support: work procedures -----------------------------------------------*/
/*
// anim_wp - do another frame of animation.
*/
static Boolean anim_wp(XtPointer appdat)
{
    Widget glw = (Widget) appdat;

    GLXwinset(XtDisplay(glw), XtWindow(glw));
    draw_frame("cdsu");
    return (False);
}


/*- support: drawing -------------------------------------------------------*/
/*
// draw_frame -
*/
static void draw_frame(char *ops)
{
    static Angle rx = 0;
    static Angle ry = 0;
    static Angle rz = 0;

    for (; *ops != '\0'; ops++) {
	switch (ops[0]) {
	case 'c':		/* clear */
	    cpack(RGB_BLACK);
	    clear();
	    break;
	case 'd':		/* draw */
	    cpack(cube_color);
	    pushmatrix();
		rotate(rz, 'z');
		rotate(ry, 'y');
		rotate(rx, 'x');
		model_cube_wire();
	    popmatrix();
	    break;
	case 's':		/* swap */
	    swapbuffers();
	    gflush();
	    break;
	case 'u':		/* update */
	    /* next angle */
	    rx = (rx + 10) % 3600;
	    ry = (ry + 10) % 3600;
	    rz = (rz + 10) % 3600;
	    break;
	default:
	    break;
	}
    }
}


/*- support: modelling primitives ------------------------------------------*/
/*
// model_cube_wire -
*/
static void model_cube_wire(void)
{
    static long v[8][3] = {
	{-1, -1, -1},
	{-1, -1,  1},
	{-1,  1,  1},
	{-1,  1, -1},
	{ 1, -1, -1},
	{ 1, -1,  1},
	{ 1,  1,  1},
	{ 1,  1, -1},
    };
    static int path[16] = {
	0, 1, 2, 3,
	0, 4, 5, 6,
	7, 4, 5, 1,
	2, 6, 7, 3
    };
    int i;

    bgnline();
    for (i=0; i<16; i++)
	v3i(v[path[i]]);
    endline();
}

/** eof *********************************************************************/
