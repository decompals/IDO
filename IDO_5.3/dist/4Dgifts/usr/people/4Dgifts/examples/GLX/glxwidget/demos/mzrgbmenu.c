/*
 *   mzrgbmenu.c:
 *
 *    This modification to mzrgbtrans.c includes the automatic mode
 *  and menu features found in zrgbmenu.c.
 *
 *					   Joel Tesler - 1991
 *
 *   zrgbmenu.c:
 *
 *    This program implements a combination of various GL features:
 *
 *       doublebuffered-RGB mode zbuffering, with a pop-up menu.
 *
 *  It further implements--either manually via LEFTMOUSE, or in an
 *  "automatic" (i.e. animation) mode format--movement of the polygons 
 *  via compound rotations to allow for continuous screen-oriented 
 *  rotations (see orient(), spin(), and draw_scene() below).  Horizontal
 *  mouse movement rotates the polygons about the y-axis where right is 
 *  positive and left is negative.  Vertical mouse movement rotates the
 *  polygons about the x-axis where down is positive and up is negative.
 *
 *                                            ratman - 1989
 *
 */

#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/keysym.h>
#include <Xm/MainW.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <Xm/Frame.h>
#include <X11/Xirisw/GlxMDraw.h>
#include <gl/gl.h>

/* Callbacks */
static void draw_scene_callback();
static void do_resize();
static void init_window();
static void quitAction(), quitCB(), arm(), disarm(), mousemove();
static void manualCB(), automaticCB();
static Widget createMenuBar();
static XtWorkProcId workProcId;

static unsigned long background;
extern unsigned long WidgetBackgroundToGlRgb();

static XtAppContext app_context;
static String fallback_resources[] = {
    "*frame*shadowType: SHADOW_IN",
    "*glwidget*width: 600",
    "*glwidget*height: 600",
    NULL
    };

static XtActionsRec actionsTable[] = {
    {"quit", quitAction},
    {"arm", arm},
    {"disarm", disarm},
    {"move", mousemove},
};

static char defaultTranslations[] =
    "<KeyUp>osfCancel:	quit() \n\
     <Btn1Down>:	arm() \n\
     <Btn1Up>:		disarm() \n\
     <Btn1Motion>:	move() \n ";

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
#define SPIN 2

int mode = 0;
int omx, mx, omy, my;                             /* old and new mouse position */
float scrnaspect;                            /* aspect ratio value         */
long zfar;               /* holds specific machine's maximum Z depth value */

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
    Widget glw, toplevel, mainw, frame, menu_bar;
    XtTranslations transTable;

    toplevel = XtAppInitialize(&app_context, "4DgiftsGlx", 
			       (XrmOptionDescList)NULL , 0,
			       (Cardinal*)&argc, 
			       (String*)argv, 
			       fallback_resources,
			       (ArgList)NULL, 0);

    XtAppAddActions(app_context, actionsTable, XtNumber(actionsTable));
    transTable = XtParseTranslationTable(defaultTranslations);
		 
    n = 0;
    mainw = XmCreateMainWindow(toplevel, "mainw", args, n);
    XtManageChild(mainw);

    menu_bar = createMenuBar(mainw);
    n = 0;
    frame = XmCreateFrame (mainw, "frame", args, n);
    XtManageChild (frame);

    n = 0;
    XtSetArg(args[n], GlxNglxConfig, glxConfig); n++;
    glw = GlxCreateMDraw(frame, "glwidget", args, n);
    XtManageChild (glw);
    XtAddCallback(glw, GlxNexposeCallback, draw_scene_callback, 0);
    XtAddCallback(glw, GlxNresizeCallback, do_resize, 0);
    XtAddCallback(glw, GlxNginitCallback, init_window, 0);
    XtOverrideTranslations(glw, transTable);

    /* Obtain the proper background pixel */
    background = WidgetBackgroundToGlRgb(glw);
    XmMainWindowSetAreas( mainw, menu_bar, NULL, NULL, NULL, frame);

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
        case SPIN:
            spin();
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


spin () {

    pushmatrix();

    loadmatrix(idmat);

    rotate(5, 'x');            
    rotate(6, 'y');           
    rotate(4, 'z');          

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
quitAction(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    exit(0);
}

static void
quitCB(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    caddr_t call_data;
{
    exit(0);
}

static void
arm(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    mode = ORIENT;
    omx = mx = event->xbutton.x;
    omy = my = event->xbutton.y;
}

static void
disarm(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    mode = NOTHING;
}

static void
mousemove(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    if (mode == ORIENT)
    {
	omx = mx;
	omy = my;
	mx = event->xbutton.x;
	my = event->xbutton.y;
	update_scene();
	draw_scene_callback(w,0,0);
    }
}

Boolean automaticWP()
{
    if (mode == SPIN)
    {
	update_scene();
	draw_scene();
	return (FALSE);  /* call the work proc again */
    }
    else
    {
	workProcId = 0;
	return (TRUE);  /* don't call the work proc again */
    }
}

static void manualCB(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    caddr_t call_data;
{
    if (workProcId)
    {
	XtRemoveWorkProc(workProcId);
	workProcId = 0;
    }
    mode = NOTHING;
}

static void automaticCB(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    caddr_t call_data;
{
    mode = SPIN;
    /* remove any existing work proc if necessary */
    if (workProcId)
    {
	XtRemoveWorkProc(workProcId);
	workProcId = 0;
    }
    workProcId = XtAppAddWorkProc(app_context,automaticWP,0);
}


static Widget
createMenuBar(parent)
Widget parent;
{
    Widget menu_bar;
    Widget cascade;
    Widget menu_pane;
    Widget button;
    Arg args[10];
    register int n;

    n = 0;
    menu_bar = XmCreateMenuBar(parent, "menu_bar", args, n);
    XtManageChild (menu_bar);

    n = 0;
    menu_pane = XmCreatePulldownMenu (menu_bar, "menu_pane", args, n);

    n = 0;
    button = XmCreatePushButton (menu_pane, "Quit", args, n);
    XtManageChild (button);
    XtAddCallback (button, XmNactivateCallback, quitCB, NULL);

    n = 0;
    XtSetArg (args[n], XmNsubMenuId, menu_pane); n++;
    cascade = XmCreateCascadeButton (menu_bar, "File", args, n);
    XtManageChild (cascade);

    n = 0;
    menu_pane = XmCreatePulldownMenu (menu_bar, "menu_pane", args, n);

    n = 0;
    button = XmCreatePushButton (menu_pane, "Manual", args, n);
    XtManageChild (button);
    XtAddCallback (button, XmNactivateCallback, manualCB, NULL);

    n = 0;
    button = XmCreatePushButton (menu_pane, "Automatic", args, n);
    XtManageChild (button);
    XtAddCallback (button, XmNactivateCallback, automaticCB, NULL);

    n = 0;
    XtSetArg (args[n], XmNsubMenuId, menu_pane); n++;
    cascade = XmCreateCascadeButton (menu_bar, "Mode", args, n);
    XtManageChild (cascade);

    return (menu_bar);
}
