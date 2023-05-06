/*
 *   azrgbinput.c:
 *
 *    This is a modification of zrgb that puts the GL window inside an
 *  Athena widget form.  It uses the input callback to receive events.
 *  It is identical to mzrgbinput.c except for the use of Athena widgets
 *  instead of Motif widgets, and for the use of the generic GlxDraw widget
 *  instead of the Motif specific GlxMDraw widget.
 *  It is based on the zrgb.c program.
 *
 *					   Joel Tesler - 1991
 *
 *   zrgb.c:
 *
 *    This program demostrates zbuffering 3 intersecting RGB polygons while
 *  in doublebuffer mode where, movement of the mouse with the LEFTMOUSE 
 *  button depressed will, rotate the 3 polygons. This is done via compound
 *  rotations allowing continuous screen-oriented rotations. (See orient(),
 *  and draw_scene() below).  Notice the effective way there is no wasted 
 *  CPU usage when the user moves the mouse out of the window without holding
 *  down LEFTMOUSE--there is no qtest being performed and so the program
 *  simply blocks on the qread statement.
 *    Press the "Esc"[ape] key to exit.  
 *     Please note that this program will not work on any 8-bit IRIS
 *  machine.
 *                                          ratman - 1989
 */

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/keysym.h>
#include <X11/Xaw/Form.h>
#include <X11/Xirisw/GlxDraw.h>
#include <gl/gl.h>

/* Callbacks */
static void draw_scene_callback();
static void do_resize();
static void init_window();
static void input();

unsigned long background;
extern unsigned long WidgetBackgroundToGlRgb();

Matrix objmat = {
    {1.0, 0.0, 0.0, 0.0},
    {0.0, 1.0, 0.0, 0.0},
    {0.0, 0.0, 1.0, 0.0},
    {0.0, 0.0, 0.0, 1.0},
};

Matrix idmat = {
    {1.0, 0.0, 0.0, 0.0},
    {0.0, 1.0, 0.0, 0.0},
    {0.0, 0.0, 1.0, 0.0},
    {0.0, 0.0, 0.0, 1.0},
};


/* Modes the program can be in */
#define NOTHING 0
#define ORIENT 1

int mode = 0;
int omx, mx, omy, my;                             /* old and new mouse position */
float scrnaspect;                            /* aspect ratio value         */
long zfar;               /* holds specific machine's maximum Z depth value */

static XtAppContext app_context;
static String fallback_resources[] = {
    "*glwidget*width: 600",
    "*glwidget*height: 600",
    NULL
    };

/* The GLX configuration parameter:
 * 	Double buffering
 *	RGB mode
 *	Z buffering
 *	nothing else special
 */
static GLXconfig glxConfig [] = {
    { GLX_NORMAL, GLX_DOUBLE, TRUE },
    { GLX_NORMAL, GLX_RGB, TRUE },
    { GLX_NORMAL, GLX_ZSIZE, GLX_NOCONFIG },
    { 0, 0, 0 }
};

main(argc, argv)
int argc;
char *argv[];
{
    Arg args[20];
    int n;
    Widget glw, toplevel, form;


    toplevel = XtAppInitialize(&app_context, "4DgiftsGlx", 
			       (XrmOptionDescList)NULL , 0,
			       (Cardinal*)&argc, 
			       (String*)argv, 
			       fallback_resources,
			       (ArgList)NULL, 0);

    n = 0;
    form = XtCreateManagedWidget("form",
				  formWidgetClass,
				  toplevel, args, n);

    n = 0;
    XtSetArg(args[n], GlxNglxConfig, glxConfig); n++;
    glw = XtCreateManagedWidget("glwidget",
				  glxDrawWidgetClass,
				  form, args, n);
    XtAddCallback(glw, GlxNexposeCallback, draw_scene_callback, 0);
    XtAddCallback(glw, GlxNresizeCallback, do_resize, 0);
    XtAddCallback(glw, GlxNginitCallback, init_window, 0);
    XtAddCallback(glw, GlxNinputCallback, input, 0);

    /* Obtain the proper background pixel */
    background = WidgetBackgroundToGlRgb(glw);

    XtRealizeWidget(toplevel);
    installColormap(toplevel,glw);

    XtAppMainLoop(app_context);

}


initialize(progname)
char *progname;
{

    long xscrnsize;              /* size of screen in x used to set globals  */
    long testifZinst;


    /*
     * This program requires the following to run:
     *  -- z buffer
     *  -- ability to do double-buffered RGB mode
     */
    /* Test for Z buffer */
    testifZinst = getgdesc(GD_BITS_NORM_ZBUFFER);
    if (testifZinst == FALSE) {
         fprintf(stderr,"BUMmer!--%s won't work on ", progname);
         fprintf(stderr,"this machine--zbuffer option not installed.\n");
         exit(0);
    }
    /* Test for double-buffered RGB */
    if (getgdesc(GD_BITS_NORM_DBL_RED) == 0) {
         fprintf(stderr,"BUMmer!--%s won't work on ", progname);
         fprintf(stderr,"this machine--not enough bitplanes.\n");
         exit(0);
        
    }

    /* Code to keep same aspec ratio as the screen */
    keepaspect(getgdesc(GD_XMMAX), getgdesc(GD_YMMAX));
    scrnaspect = (float)getgdesc(GD_XMMAX)/(float)getgdesc(GD_YMMAX);

    doublebuffer();
    RGBmode();
    zbuffer(TRUE);
    glcompat(GLC_ZRANGEMAP, 0);
    zfar = getgdesc(GD_ZMAX);

}

static void
draw_scene_callback(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    caddr_t call_data;
{
    GLXwinset(XtDisplay(w), XtWindow(w));
    draw_scene();
}

static void
init_window(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    caddr_t call_data;
{
    GLXwinset(XtDisplay(w), XtWindow(w));
    initialize();
}

static void
do_resize(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    GlxDrawCallbackStruct *call_data;
{
    GLXwinset(XtDisplay(w), XtWindow(w));
    viewport(0, (Screencoord) call_data->width-1,
	     0, (Screencoord) call_data->height-1);
    draw_scene_callback(w, 0, 0);
}

update_scene() {

    switch (mode) {

        case ORIENT:
            orient();
            break;
    }
}


orient () {

    pushmatrix();

    loadmatrix(idmat);

    rotate(mx-omx, 'y');
    /* Note:  The following example had omy-my in the original GL program.
     * However, since we are using X coordinates, we reverse this.
     */
    rotate(my-omy, 'x');

    multmatrix(objmat);
    getmatrix(objmat);

    popmatrix();
}


draw_scene() {

    czclear(background, zfar);

    perspective(400, scrnaspect, 30.0, 60.0);
    translate(0.0, 0.0, -40.0);
    multmatrix(objmat);
    rotate(-580, 'y');    /* skews original view to show all polygons */
    draw_polys();

    swapbuffers();
    gflush();
}


float polygon1[3][3] = { {-10.0, -10.0,   0.0,},
                         { 10.0, -10.0,   0.0,},
                         {-10.0,  10.0,   0.0,} };

float polygon2[3][3] = { {  0.0, -10.0, -10.0,},
                         {  0.0, -10.0,  10.0,},
                         {  0.0,   5.0, -10.0,} };

float polygon3[4][3] = { {-10.0,   6.0,   4.0,},
                         {-10.0,   3.0,   4.0,},
                         {  4.0,  -9.0, -10.0,},
                         {  4.0,  -6.0, -10.0,} };

draw_polys() {

    bgnpolygon();
    cpack(0x00000000);
    v3f(&polygon1[0][0]);
    cpack(0x007F7F7F);
    v3f(&polygon1[1][0]);
    cpack(0x00FFFFFF);
    v3f(&polygon1[2][0]);
    endpolygon();

    bgnpolygon();
    cpack(0x0000FFFF);
    v3f(&polygon2[0][0]);
    cpack(0x007FFF00);
    v3f(&polygon2[1][0]);
    cpack(0x00FF0000);
    v3f(&polygon2[2][0]);
    endpolygon();

    bgnpolygon();
    cpack(0x0000FFFF);
    v3f(&polygon3[0][0]);
    cpack(0x00FF00FF);
    v3f(&polygon3[1][0]);
    cpack(0x00FF0000);
    v3f(&polygon3[2][0]);
    cpack(0x00FF00FF);
    v3f(&polygon3[3][0]);
    endpolygon();
}

/* Process all Input callbacks*/
static void
input(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    GlxDrawCallbackStruct *call_data;
{
    char buffer[1];
    KeySym keysym;

    switch(call_data->event->type)
    {
    case KeyRelease:
	/* It is necessary to convert the keycode to a keysym before
	 * it is possible to check if it is an escape
	 */
	if (XLookupString(call_data->event,buffer,1,&keysym,NULL) == 1 &&
	    keysym == (KeySym)XK_Escape)
	    exit(0);
    case ButtonPress:
	switch(call_data->event->xbutton.button)
	{
	case Button1:
	    mode = ORIENT;
	    omx = mx = call_data->event->xbutton.x;
	    omy = my = call_data->event->xbutton.y;
	    break;
	}
	break;
    case ButtonRelease:
	switch(call_data->event->xbutton.button)
	{
	case Button1:
	    mode = NOTHING;
	    break;
	}
	break;
    case MotionNotify:
	if (mode == ORIENT && call_data->event->xmotion.state & Button1Mask)
	{
	    omx = mx;
	    omy = my;
	    mx = call_data->event->xbutton.x;
	    my = call_data->event->xbutton.y;
	    update_scene();
	    draw_scene_callback(w,0,0);
	}
	break;
    }
	
}
