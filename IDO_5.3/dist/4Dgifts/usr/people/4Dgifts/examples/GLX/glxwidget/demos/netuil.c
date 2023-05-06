/*
 * This is the same as the program network.c, except that it uses uil.
 * The GlxDraw widget is treated as a user defined widget.
 * See the file netuil.uil and glxuil.uil for the uil code.
 *
 *				Michael Gold and Joel Tesler - 1991
 */
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Mrm/MrmAppl.h>
#include <X11/Xirisw/GlxMDraw.h>

#include <math.h>

#define RAD(x) ((double) (x * M_PI / 1800))
#define FAR 0x7fff
#define INITIAL_INC 60
#define PULSE 50
#define RADIUS 15

Angle ang = 0;
int interval = 10;
int step = INITIAL_INC;

long
    blackcol[3] = {0, 0, 0},
    bluecol[3] = {0, 0, 255},
    greencol[3] = {0, 255, 0},
    yellowcol[3] = {255, 255, 0},
    redcol[3] = {255, 0, 0},
    red2col[3] = {225, 0, 0},
    whitecol[3] = {255, 255, 255},
    purplecol[3] = {160, 30, 120},
    tancol[3] = {214, 196, 171};

long
    background[3];

static XtCallbackProc draw_scene();
static XtCallbackProc do_resize();
static XtCallbackProc init_window();
static XtCallbackProc set_inc();
static XtCallbackProc quit();
static XtTimerCallbackProc handle_timeout();
static XtAppContext app_context;
    
/* The GLX configuration parameter:
 * 	Double buffering
 *	Color Index mode (default so unspecified)
 *	nothing else special
 */
static GLXconfig glxConfig [] = {
    { GLX_NORMAL, GLX_DOUBLE, TRUE },
    { GLX_NORMAL, GLX_RGB, TRUE },
    { GLX_NORMAL, GLX_ZSIZE, GLX_NOCONFIG },
    { 0, 0, 0 }
};


/* Uil stuff */
static MrmHierarchy s_MrmHierarchy;

static char *mrm_filename_vec[] = {
    "netuil.uid",
};

static MrmRegisterArg register_vector[] = {
    {"glx_config", (caddr_t)glxConfig},
    {"quit", (caddr_t)quit},
    {"set_inc", (caddr_t)set_inc},
    {"draw_scene", (caddr_t)draw_scene},
    {"do_resize", (caddr_t)do_resize},
    {"init_window", (caddr_t)init_window},
};

Widget toplevel;	/* makes it easy to install the colormap */

main(argc, argv)
    char **argv;
{
    Widget form=NULL;
    MrmType *dummy_class;
    
    MrmInitialize();
    toplevel = XtAppInitialize(&app_context, "4DgiftsGlx", 
			       (XrmOptionDescList)NULL , 0,
			       (Cardinal*)&argc, 
			       (String*)argv, 
			       (String*)NULL,
			       (ArgList)NULL, 0);

    /* Since the GlxMDraw is a user defined widget, we need to register
     * its class
     */
    if (MrmRegisterClass ( MrmwcUnknown, "GlxMDraw", "GlxCreateMDraw",
			  GlxCreateMDraw, &dummy_class) != MrmSUCCESS)
	XtError("Cannot register GlxMDraw with mrm");
	
    if (MrmOpenHierarchy(1,mrm_filename_vec,NULL,&s_MrmHierarchy) != MrmSUCCESS)
	XtError ("Cannot open Mrm hierarchy");
    MrmRegisterNames (register_vector, XtNumber(register_vector));
    if (MrmFetchWidget (s_MrmHierarchy,"form",toplevel,
			&form, &dummy_class) != MrmSUCCESS)
	XtError ("Cannot fetch Mrm widget hierarchy");
    XtManageChild(form);

    XtRealizeWidget(toplevel);

    XtAppMainLoop(app_context);
}

static XtCallbackProc
quit(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    caddr_t call_data;
{
    exit (0);
}

static XtCallbackProc
draw_scene(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    caddr_t call_data;
{
    GLXwinset(XtDisplay(w), XtWindow(w));
    drawscene(ang);
    gflush();
}

drawscene(ang)
    Angle ang;
{
    pushmatrix();
    c3i(background);
    clear();
    ring(ang);
    workstation(900);
    workstation(2100);
    workstation(3300);
    popmatrix();
    swapbuffers(); 
}


ring(ang)
    int ang;
{
    linewidth(5);
    c3i(yellowcol);
    circ(0, 0, RADIUS);
    c3i(purplecol);
    arc(0, 0, RADIUS, ang-PULSE/2, ang+PULSE/2);
    linewidth(1);
}

vert3f(x, y, z)
    float x, y, z;
{
    float v[3];

    v[0] = x; v[1] = y; v[2] = z;
    v3f(v);
}


workstation(drawang)
    Angle drawang;
{
    float x,y;
    pushmatrix();
    x = (float) cos(RAD(drawang)) * RADIUS;
    y = (float) sin(RAD(drawang)) * RADIUS;
    translate(x, y, 0.);
    rotate(200,'x');
    rotate(300,'y');
    scale(0.3, 0.3, 0.3);
    drawbase();
    drawmonitor(drawang);
    drawkeybd();
    popmatrix();
}

drawbase()
{
/* front of base */
    c3i(tancol);
    bgnpolygon();    
    vert3f(-8., -10., 1.);
    vert3f(8., -10., 1.);
    vert3f(8., -7., 1.);
    vert3f(-8., -7., 1.);
    endpolygon();
    c3i(blackcol);
    bgnclosedline();
    vert3f(-8., -10., 1.);
    vert3f(8., -10., 1.);
    vert3f(8., -7., 1.);
    vert3f(-8., -7., 1.);
    endclosedline();
/* left side of the base */
    c3i(tancol);
    bgnpolygon();    
    vert3f(-8., -10., 1.);
    vert3f(-8., -10., -10.);
    vert3f(-8., -7., -10.);
    vert3f(-8., -7., 1.);
    endpolygon();
    c3i(blackcol);
    bgnclosedline();
    vert3f(-8., -10., 1.);
    vert3f(-8., -10., -10.);
    vert3f(-8., -7., -10.);
    vert3f(-8., -7., 1.);
    endclosedline();
/* top of base */
    c3i(tancol);
    bgnpolygon();
    vert3f(-8., -7., 1.);
    vert3f(-8., -7., -10.);
    vert3f(8., -7., -10.);
    vert3f(8., -7., 1.);
    endpolygon();
    c3i(blackcol);
    bgnclosedline();
    vert3f(-8., -7., 1.);
    vert3f(-8., -7., -10.);
    vert3f(8., -7., -10.);
    vert3f(8., -7., 1.);
    endclosedline();
}

drawmonitor(drawang)
    Angle drawang;
{
/* front of monitor */
    c3i(tancol);
    bgnpolygon();
    vert3f(-8.,8.,1.);
    vert3f(-8.,-6.,1.);
    vert3f(8.,-6.,1.);
    vert3f(8.,8.,1.);
    endpolygon();
    c3i(blackcol);
    bgnclosedline();
    vert3f(-8.,8.,1.);
    vert3f(-8.,-6.,1.);
    vert3f(8.,-6.,1.);
    vert3f(8.,8.,1.);
    endclosedline();
/* top of monitor */
    c3i(tancol);
    bgnpolygon();
    vert3f(-8.,8.,1.);
    vert3f(-7.,7.,-9.);
    vert3f(7.,7.,-9.);
    vert3f(8.,8.,1.);
    endpolygon();
    c3i(blackcol);
    bgnclosedline();
    vert3f(-8.,8.,1.);
    vert3f(-7.,7.,-9.);
    vert3f(7.,7.,-9.);
    vert3f(8.,8.,1.);
    endclosedline();
/* left side of monitor */
    c3i(tancol);
    bgnpolygon();
    vert3f(-8., 8., 1.);
    vert3f(-7., 7., -9.);
    vert3f(-7., -5., -9.);
    vert3f(-8., -6., 1.);
    endpolygon();
    c3i(blackcol);
    bgnclosedline();
    vert3f(-8., 8., 1.);
    vert3f(-7., 7., -9.);
    vert3f(-7., -5., -9.);
    vert3f(-8., -6., 1.);
    endclosedline();
/* screen */
    if (abs(drawang - ang) < 150)
	c3i(purplecol);
    else
	c3i(blackcol);
    bgnpolygon();
    vert3f(-6.,6.,1.);
    vert3f(-6.,-4.,1.);
    vert3f(6.,-4.,1.);
    vert3f(6.,6.,1.);
    endpolygon();
}


drawkeybd()
{
/* side of keybd */
    c3i(tancol);
    bgnpolygon();
    vert3f(-8., -10., 3.);
    vert3f(-8., -10., 10.);
    vert3f(-8., -9., 10.);
    vert3f(-8., -8., 3.);
    endpolygon();
    c3i(blackcol);
    bgnclosedline();
    vert3f(-8., -10., 3.);
    vert3f(-8., -10., 10.);
    vert3f(-8., -9., 10.);
    vert3f(-8., -8., 3.);
    endclosedline();
/* top of keybd */
    c3i(tancol);
    bgnpolygon();
    vert3f(-8., -9., 10.);
    vert3f(-8., -8., 3.);
    vert3f(8., -8., 3.);
    vert3f(8., -9., 10.);
    endpolygon();
    c3i(blackcol);
    bgnclosedline();
    vert3f(-8., -9., 10.);
    vert3f(-8., -8., 3.);
    vert3f(8., -8., 3.);
    vert3f(8., -9., 10.);
    endclosedline();
/* front of keybd */
    c3i(tancol);
    bgnpolygon();
    vert3f(-8., -9., 10.);
    vert3f(8., -9., 10.);
    vert3f(8., -10., 10.);
    vert3f(-8., -10., 10.);
    endpolygon();
    c3i(blackcol);
    bgnclosedline();
    vert3f(-8., -9., 10.);
    vert3f(8., -9., 10.);
    vert3f(8., -10., 10.);
    vert3f(-8., -10., 10.);
    endclosedline();
}

static XtTimerCallbackProc
handle_timeout(w)
    Widget w;
{
    ang = (ang + step) % 3600;
    XtAppAddTimeOut(app_context, interval, handle_timeout, w);
    draw_scene(w, 0, 0);
}


static XtCallbackProc
init_window(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    caddr_t call_data;
{
    GLXwinset(XtDisplay(w), XtWindow(w));
    doublebuffer();
    RGBmode();
    perspective(50, 1.0, 400.0, 600.0);
    lookat(0., 0., 500., 0., 0., 0., 0.);

    /* In the pure Motif program we did this in main.  I have moved it
     * to here for the uil version, because we have easy access to the
     * widget
     */
    /* Obtain the proper background pixel */
    WidgetBackgroundToGlC3i(w,background);
    XtAppAddTimeOut(app_context, interval, handle_timeout, w);
    installColormap(toplevel,w);
}

static XtCallbackProc
do_resize(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    GlxDrawCallbackStruct *call_data;
{
    GLXwinset(XtDisplay(w), XtWindow(w));
    viewport(0, (Screencoord) call_data->width-1,
	     0, (Screencoord) call_data->height-1);
    draw_scene(w, 0, 0);
}

static XtCallbackProc
set_inc(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    XmScrollBarCallbackStruct *call_data;
{
    step = call_data->value;
}
