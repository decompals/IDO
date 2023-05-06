/*
 *   mzcmapmenu.c:
 *
 *    This modification to mzrgbmenu.c uses a colormap as in
 *  zcmapmenu.c.  One difference is that the original GL program
 *  allocated colors out of the GL colormap when it started,
 *  and restored them when it exited.  Instead, this program
 *  allocates a separate X colormap and allocates the colors out
 *  of there.  Only high colors are used out of that map,
 *  permitting the Motif colors to remain out of the low part from
 *  the default x colormap.
 *
 *					   Joel Tesler - 1991
 *
 *   zcmapmenu.c:
 *
 *     This program implements a combination of various GL features:
 *
 *       doublebuffered color-map mode zbuffering, with a pop-up menu.
 *
 *   as well as movement of the polygons via compound rotations to allow for
 *   continuous screen-oriented rotations (see orient(), spin(), and 
 *   draw_scene() below).  Horizontal/vertical mouse movement rotates the 
 *   polygons about the y/x -axes.
 *
 *   This program also demonstrates a standard approach to writing code
 *   that does not eat up CPU cycles in polling loops if the mouse is 
 *   elsewhere than over this window and/or if the mouse isn't moving.  
 *   (Past examples have included qtest() or getbutton() cycling that can 
 *   cost a lot of CPU time).  See skeleton.c in this same directory for 
 *   the "template" standalone program that was used to implement the
 *   catching and processing of input from the GL event queue.
 *
 *   see the README.zcmapmenu for more implementation details.
 *
 *                                    dave ratcliffe - 1989
 *
 */

#include <stdio.h>
#include <sys/types.h>
#include <malloc.h>
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

#define STARTINDEX  640
#define RAMPSIZE 32	/* number of pixels per ramp */
#define RAMP1 STARTINDEX
#define RAMP2 RAMP1+RAMPSIZE
#define RAMP3 RAMP2+RAMPSIZE
#define BACKGROUND RAMP3+RAMPSIZE

/* Modes the program can be in */
#define NOTHING 0
#define ORIENT 1
#define SPIN 2

int mode = 0;
int omx, mx, omy, my;                        /* old and new mouse position */
float scrnaspect;                            /* aspect ratio value         */
long zfar;               /* holds specific machine's maximum Z depth value */

Widget glw;            /* Make this global.  rampup will need it to obtain */
                       /* display and colormap.                            */

/* The GLX configuration parameter:
 * 	Double buffering
 *	Color Index mode (default so unspecified)
 *	Z buffering
 *	nothing else special
 */
static GLXconfig glxConfig [] = {
    { GLX_NORMAL, GLX_DOUBLE, TRUE },
    { GLX_NORMAL, GLX_ZSIZE, GLX_NOCONFIG },
    { 0, 0, 0 }
};

main(argc, argv)
int argc;
char *argv[];
{
    Arg args[20];
    int n;
    Widget toplevel, mainw, frame, menu_bar;
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

    XmMainWindowSetAreas( mainw, menu_bar, NULL, NULL, NULL, frame);

    XtRealizeWidget(toplevel);
    installColormap(toplevel,glw);

    XtAppMainLoop(app_context);

}


/* Extract the background from the widget's resources and install it in
 * the new colormap in pixel BACKGROUND */
setupBackground(widget)
Widget widget;
{
    Arg args[10];
    int n;
    Pixel xbg;		/* x background pixel */
    Colormap pcolormap,wcolormap;
    XColor xcolor;

    /* First get the background pixel from the widget. */
    n = 0;
    XtSetArg(args[n], XtNbackground, &xbg); n++;
    XtSetArg(args[n], XtNcolormap, &wcolormap); n++;
    XtGetValues(widget, args, n);

    /* Now get the colormap from the parent.  We can't use the widget's
     * colormap because it might not contain the same colors as the
     * colors that the background color were allocated from, so we use it's
     * parent
     */
    n = 0;
    XtSetArg(args[n], XtNcolormap, &pcolormap); n++;
    XtGetValues(XtParent(widget), args, n);

    /* Now obtain RGB values */
    xcolor.flags = DoRed | DoGreen | DoBlue;
    xcolor.pixel = xbg;
    XQueryColor (XtDisplay(widget), pcolormap, &xcolor);

    /* Modify the pixel value to BACKGROUND, and store in the
     * widget's colormap
     */
    xcolor.pixel = BACKGROUND;
    XStoreColor(XtDisplay(widget), wcolormap, &xcolor);
}

initialize(progname)
char *progname;
{

    long xscrnsize;              /* size of screen in x used to set globals  */
    long testifZinst;
    register int i, j;
    short redval, greval, bluval;


    /*
     * This program requires the following to run:
     *  -- z buffer
     *  -- ability to write color indices STARTINDEX - ENDINDEX
     *     in double-buffered colorindex mode
     */
    /* Test for Z buffer */
    testifZinst = getgdesc(GD_BITS_NORM_ZBUFFER);
    if (testifZinst == FALSE) {
         fprintf(stderr,"BUMmer!--%s won't work on ", progname);
         fprintf(stderr,"this machine--zbuffer option not installed.\n");
         exit(0);
    }
    /* Test for enough color bitplanes */
    if ((1 << getgdesc(GD_BITS_NORM_DBL_CMODE)) <= BACKGROUND) {
         fprintf(stderr,"BUMmer!--%s won't work on ", progname);
         fprintf(stderr,"this machine--not enough color bitplanes.\n");
         exit(0);
        
    }

    /* Code to keep same aspec ratio as the screen */
    keepaspect(getgdesc(GD_XMMAX), getgdesc(GD_YMMAX));
    scrnaspect = (float)getgdesc(GD_XMMAX)/(float)getgdesc(GD_YMMAX);

    doublebuffer();
    zbuffer(TRUE);
    glcompat(GLC_ZRANGEMAP, 0);
    zfar = getgdesc(GD_ZMAX);

    /*
     * stay from possible z wrap around during iteration
     */
    lsetdepth(100, zfar - 100);

    buildmap();
}

buildmap() {

    /* now build our 3 color ramps of 32 shades that make ranges of: 
     *     32-63 :    YELLOW to BLUE
     *     64-95 :    CYAN   to RED
     *     96-127:    GREEN  to MAGENTA
     */
    rampup(RAMP1, RAMPSIZE, 255,   0, 255,   0,   0, 255); 
    rampup(RAMP2, RAMPSIZE,   0, 255,   0, 255,   0, 255);
    rampup(RAMP3, RAMPSIZE,   0, 255,   0,   0,   0, 255);
}


#define round(n)     ((int) (n + 0.5))
/*
 *  rampup:
 *    make a bi-linear interpolated color ramp taking as input a pointer
 *    to storage for the first pixel and maximum values for red, green,
 *    and blue
 */
rampup(first,length,minR,maxR,minG,maxG,minB,maxB)
int first;   /* pointer to storage for first pixel */
int length;    /* number of pixels */
unsigned short minR, maxR, minG, maxG, minB, maxB;     /* lo/hi rgb vals */
{
    unsigned short len_red, len_green, len_blue, /* length of each color */
                   i;                     /* counter for number of steps */

    float rdx, gdx, bdx,                      /* sizes of rgb increments */
          r, g, b,                             /* a position on the ramp */
          steps;                    /* # of steps along the ramp @ which */
                                   /* intensity assignments will be made */
    XColor *colors;                  /* storage for the allocated colors */
    int n;
    Arg args[10];
    Colormap xcolormap;

    colors = (XColor *)malloc(length * sizeof(XColor)); 

    n = 0;
    XtSetArg(args[n], XtNcolormap, &xcolormap); n++;
    XtGetValues(glw, args, n);                  /* get the colormap */
	
    steps = (float) (length);                  /*determine length of ramp*/

    len_red   = (maxR - minR) + 1;            /* determine length of red */
    len_green = (maxG - minG) + 1;          /* determine length of green */
    len_blue  = (maxB - minB) + 1;           /* determine length of blue */

    rdx = (float) len_red   / steps;                     /* compuke step */
    gdx = (float) len_green / steps;                     /* sizes of r g */
    bdx = (float) len_blue  / steps;                     /* and b values */

    r = minR;                                         /* assign starting */
    g = minG;                                        /* indices for each */
    b = minB;                                        /* color value      */

    for (i = 0; i < length; i++) {
	colors[i].pixel = first+i;
        colors[i].red = round(r) << 8;                     /* round out */
        colors[i].green = round(g) << 8;                   /* given r g */
        colors[i].blue = round(b) << 8;                    /* b value   */
	colors[i].flags = DoRed|DoGreen|DoBlue;
        r += rdx;                                          /* increment */
        g += gdx;                                          /* color in- */
        b += bdx;                                          /* dices     */
    }
    XStoreColors(XtDisplay(glw), xcolormap, colors, length);

    free (colors);                                    /* return storage */
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
    XVisualInfo *visualInfo;
    Colormap colormap;
    Arg args[10];
    int n;
    
    GLXwinset(XtDisplay(w), XtWindow(w));

    /* Replace the GL colormap with one of our own.  The original GL
     * demo overwrote some of the GL colors and restored them on exit.
     * Rather than do that, we allocate our own colormap.
     */
    /* first get the visual */
    n = 0;
    XtSetArg(args[n], XtNvisual, &visualInfo); n++;
    XtGetValues(w, args, n);
    /* create a new colormap */
    colormap = XCreateColormap(XtDisplay(w), XtWindow(w),
			       visualInfo->visual, AllocAll);
    /* Add the colormap to the window */
    n = 0;
    XtSetArg(args[n], XtNcolormap, colormap); n++;
    XtSetValues(w, args, n);
    setupBackground(glw);
    
    /* initialize the GL */
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

    czclear(BACKGROUND, zfar);

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
    color(RAMP2);
    v3f(&polygon1[0][0]);
    color(RAMP2+RAMPSIZE-1);
    v3f(&polygon1[1][0]);
    color(RAMP2+(RAMPSIZE/2));
    v3f(&polygon1[2][0]);
    endpolygon();

    bgnpolygon();
    color(RAMP3+RAMPSIZE-1);
    v3f(&polygon2[0][0]);
    color(RAMP3+(RAMPSIZE/2));
    v3f(&polygon2[1][0]);
    color(RAMP3);
    v3f(&polygon2[2][0]);
    endpolygon();

    bgnpolygon();
    color(RAMP1);
    v3f(&polygon3[0][0]);
    color(RAMP1+RAMPSIZE/2);
    v3f(&polygon3[1][0]);
    color(RAMP1+RAMPSIZE-1);
    v3f(&polygon3[2][0]);
    color(RAMP1+RAMPSIZE/2);
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
