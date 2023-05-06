/*
 * This program uses mixed GL and Motif to show a schematic of a network.
 * A motif scrollbar controls the speed of data transfer in the Gl Drawn
 * network.
 *
 *				Michael Gold and Joel Tesler - 1991
 */
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Form.h>
#include <Xm/ScrollBar.h>
#include <Xm/PushB.h>
#include <Xm/Frame.h>
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


main(argc, argv)
    char **argv;
{
    Arg args[20];
    int nargs = 0;
    Widget glw, toplevel, form, frame, slider, pushb;

    toplevel = XtAppInitialize(&app_context, "4DgiftsGlx", 
			       (XrmOptionDescList)NULL , 0,
			       (Cardinal*)&argc, 
			       (String*)argv, 
			       (String*)NULL,
			       (ArgList)NULL, 0);

    nargs = 0;
    XtSetArg(args[nargs], XtNwidth, 700); nargs++;
    XtSetArg(args[nargs], XtNheight, 700); nargs++;
    form = XtCreateManagedWidget("form",
				  xmFormWidgetClass,
				  toplevel, args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], XmNtopAttachment, XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs], XmNleftAttachment, XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs], XmNtopOffset, 10); nargs++;
    XtSetArg(args[nargs], XmNleftOffset, 30); nargs++;
    pushb = XtCreateManagedWidget("Quit",
				   xmPushButtonWidgetClass,
				   form, args, nargs);
    XtAddCallback(pushb, XmNactivateCallback, quit,0);
    nargs = 0;
    XtSetArg(args[nargs], XtNx, 430); nargs++;
    XtSetArg(args[nargs], XtNy, 30); nargs++;
    XtSetArg(args[nargs], XtNwidth, 20); nargs++;
    XtSetArg(args[nargs], XtNheight, 420); nargs++;
    XtSetArg(args[nargs], XmNbottomAttachment, XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs], XmNtopAttachment, XmATTACH_WIDGET); nargs++;
    XtSetArg(args[nargs], XmNrightAttachment, XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs], XmNrightOffset, 30); nargs++;
    XtSetArg(args[nargs], XmNtopOffset, 10); nargs++;
    XtSetArg(args[nargs], XmNtopWidget, pushb); nargs++;
    XtSetArg(args[nargs], XmNbottomOffset, 30); nargs++;
    XtSetArg(args[nargs], XmNminimum, 0); nargs++;
    XtSetArg(args[nargs], XmNmaximum, 500); nargs++;
    XtSetArg(args[nargs], XmNvalue, step); nargs++;
    XtSetArg(args[nargs], XmNsliderSize, 25); nargs++;
    XtSetArg(args[nargs], XmNincrement, 10); nargs++;
    XtSetArg(args[nargs], XmNpageIncrement, 50); nargs++;
    
    slider = XtCreateManagedWidget("slider",
				  xmScrollBarWidgetClass,
				  form, args, nargs);

    XtAddCallback(slider, XmNdecrementCallback, set_inc,0);
    XtAddCallback(slider, XmNincrementCallback, set_inc,0);
    XtAddCallback(slider, XmNdragCallback, set_inc,0);
    XtAddCallback(slider, XmNpageIncrementCallback, set_inc,0);
    XtAddCallback(slider, XmNpageDecrementCallback, set_inc,0);

    nargs = 0;
    XtSetArg(args[nargs], XtNx, 30); nargs++;
    XtSetArg(args[nargs], XtNy, 30); nargs++;
    XtSetArg(args[nargs], XtNwidth, 350); nargs++;
    XtSetArg(args[nargs], XtNheight, 420); nargs++;
    XtSetArg(args[nargs], XmNbottomAttachment, XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs], XmNtopAttachment, XmATTACH_WIDGET); nargs++;
    XtSetArg(args[nargs], XmNtopWidget, pushb); nargs++;
    XtSetArg(args[nargs], XmNleftAttachment, XmATTACH_FORM); nargs++;
    XtSetArg(args[nargs], XmNleftOffset, 30); nargs++;
    XtSetArg(args[nargs], XmNtopOffset, 10); nargs++;
    XtSetArg(args[nargs], XmNbottomOffset, 30); nargs++;
    XtSetArg(args[nargs], XmNrightAttachment, XmATTACH_WIDGET); nargs++;
    XtSetArg(args[nargs], XmNrightWidget, slider); nargs++;
    XtSetArg(args[nargs], XmNrightOffset, 30); nargs++;
    XtSetArg(args[nargs], XmNshadowType, XmSHADOW_IN); nargs++;
    frame = XtCreateManagedWidget("frame",
				  xmFrameWidgetClass,
				  form, args, nargs);

    nargs = 0;
    XtSetArg(args[nargs], GlxNglxConfig, glxConfig); nargs++;
    glw = XtCreateManagedWidget("glwidget",
				  glxMDrawWidgetClass,
				  frame, args, nargs);
    XtAddCallback(glw, GlxNexposeCallback, draw_scene, 0);
    XtAddCallback(glw, GlxNresizeCallback, do_resize, 0);
    XtAddCallback(glw, GlxNginitCallback, init_window, 0);
    XtAppAddTimeOut(app_context, interval, handle_timeout, glw);

    /* Obtain the proper background pixel */
    WidgetBackgroundToGlC3i(glw,background);

    XtRealizeWidget(toplevel);
    installColormap(toplevel,glw);

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
