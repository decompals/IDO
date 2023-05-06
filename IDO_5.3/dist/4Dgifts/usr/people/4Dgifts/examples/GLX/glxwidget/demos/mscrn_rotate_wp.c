
/*
 *                               mscrn_rotate_wp.c
 *
 *     This is a modification to mscrn_rotate.c that uses workprocs to
 *  continually rotate the cube while a button is pressed rather than
 *  waiting for individual events.  It leads to a much faster rotating
 *  cube.
 *
 *			   Joel Tesler - 1991
 *
 *  Based on original scrn_rotate.c by
 *
 *                          Paul Mlyniec and David Ratcliffe - 1986
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Frame.h>
#include <X11/Xirisw/GlxMDraw.h>
#include <gl/gl.h>
#include "copycmap.h"

Coord ident [4][4] =     { 1.0, 0.0, 0.0, 0.0,    /* identity matrix */
                           0.0, 1.0, 0.0, 0.0,
                           0.0, 0.0, 1.0, 0.0,
                           0.0, 0.0, 0.0, 1.0};

static Coord cm [4][4] = { 1.0, 0.0, 0.0, 0.0,    /* cumulative matrix */
                           0.0, 1.0, 0.0, 0.0,
                           0.0, 0.0, 1.0, 0.0,
                           0.0, 0.0, 0.0, 1.0};

/*  Define the sides of the cube in world coordinates. */

static Coord pfrnt[4][3] = {{    0.0,    0.0,    0.0},
                            {  100.0,    0.0,    0.0},
                            {  100.0,  100.0,    0.0},
                            {    0.0,  100.0,    0.0}};
    
static Coord pback[4][3] = {{    0.0,    0.0, -100.0},
                            {    0.0,  100.0, -100.0},
                            {  100.0,  100.0, -100.0},
                            {  100.0,    0.0, -100.0}};
    
static Coord ptop[4][3] =  {{    0.0,  100.0,    0.0},
                            {  100.0,  100.0,    0.0},
                            {  100.0,  100.0, -100.0},
                            {    0.0,  100.0, -100.0}};
    
static Coord pbot[4][3] =  {{    0.0,    0.0,    0.0},
                            {    0.0,    0.0, -100.0},
                            {  100.0,    0.0, -100.0},
                            {  100.0,    0.0,    0.0}};
    
static Coord prsid[4][3] = {{  100.0,    0.0,    0.0},
                            {  100.0,    0.0, -100.0},
                            {  100.0,  100.0, -100.0},
                            {  100.0,  100.0,    0.0}};
    
static Coord plsid[4][3] = {{    0.0,    0.0,    0.0},
                            {    0.0,  100.0,    0.0},
                            {    0.0,  100.0, -100.0},
                            {    0.0,    0.0, -100.0}};

Coord x, y, z;
Angle rx, ry, rz;
float norm_dot();

String fallback_resources[] = {
    "*geometry: =400x400",
    "*frame*shadowType: SHADOW_IN",
    NULL
};

/* information for setting up colormap */
static Colorindex red, green, yellow, blue, magenta, cyan;
static Colorindex background;
/* The following structure is declared in copycmap.h and is used by
 * CopyGlColormap (in this demo directory) to set up a clormap
 * with the specified colors.  Note that while we are using GLXC_NAMED
 * to look up named colors, we could also use GLXC_ABSOLUTE and specify
 * hardcoded pixel numbers 1-6 to copy from the X colormap.
 */
struct glxcColorInfo colorInfo[] =
{
    { GLXC_NAMED, (caddr_t)"red", &red},
    { GLXC_NAMED, (caddr_t)"green", &green},
    { GLXC_NAMED, (caddr_t)"yellow", &yellow},
    { GLXC_NAMED, (caddr_t)"blue", &blue},
    { GLXC_NAMED, (caddr_t)"magenta", &magenta},
    { GLXC_NAMED, (caddr_t)"cyan", &cyan},
    { GLXC_RESOURCE, (caddr_t)XmNbackground, &background},
};

static XtAppContext app_context;

/* forward declarations of callbacks */
static void exposeCB();
static void resizeCB();
static void initCB();
/* forward declarations of actions */
static void quitAction();
static void resetAction();
static void translateAction();
static void rotateAction();
static void doneAction();
/* forward declaration of workproc */
static Boolean workProc();
/* other forward declarations */
static void initialize_gl();

/*
 * Set up the actions table.  Note that we have only one action each for
 * translate and rotate, and let the translations pass in the axis and
 * direction
 */
static XtActionsRec actionsTable[] = {
    {"quit", quitAction},
    {"reset", resetAction},
    {"translate", translateAction},
    {"rotate", rotateAction},
    {"done", doneAction},
};

/*
 * Set up the default translations.
 */
static char defaultTranslations[] =
    "<Key>osfCancel: quit()          \n\
     <KeyUp>:        done()          \n\
     <Key>f:         translate(z 20) \n\
     <Key>KP_1:      translate(z 20) \n\
     <Key>b:         translate(z -20) \n\
     <Key>KP_3:      translate(z -20) \n\
     <Key>KP_2:      rotate(x -100)  \n\
     <Key>KP_4:      rotate(y 100)   \n\
     <Key>KP_6:      rotate(y -100)  \n\
     <Key>KP_7:      rotate(z 100)   \n\
     <Key>KP_8:      rotate(x 100)   \n\
     <Key>KP_9:      rotate(z -100)  \n\
     <Key>KP_5:      reset()";

/* The GLX configuration parameter:
 * 	Double buffering
 *	color index (default so unspecified)
 *	nothing else special
 */
static GLXconfig glxConfig [] = {
    { GLX_NORMAL, GLX_DOUBLE, TRUE },
    { 0, 0, 0 }
};

/*
 * Information relating to the workproc actions.  Some of this could
 * be passed directly to the workproc as a parameter, however the
 * cleanup code is done by actions and needs to know some of this 
 * information, so it must be global.
 */
struct {
    XtWorkProcId workProcId;	/* if 0, no workProc in progress */
    void (*doit)();		/* the function to rotate or translate */
    void (*cleanup)();		/* a cleanup procedure if needed */
    double parm;		/* rotation or translation amount */
    char axis;
    union {
	Coord *cp;		/* pointer to coord to change */
	Angle *ap;		/* pointer to angle to change */
    } un;
} workProcInfo;

main(argc, argv)
int argc;
char *argv[];
{
    Arg args[20];
    int n = 0;
    Widget toplevel, frame, glw;
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
    frame = XtCreateManagedWidget("frame",
				  xmFrameWidgetClass,
				  toplevel, args, n);
    n = 0;
    XtSetArg(args[n], GlxNglxConfig, glxConfig); n++;
    glw = XtCreateManagedWidget("glwidget",
				  glxMDrawWidgetClass,
				  frame, args, n);
    XtAddCallback(glw, GlxNexposeCallback, exposeCB, 0);
    XtAddCallback(glw, GlxNresizeCallback, resizeCB, 0);
    XtAddCallback(glw, GlxNginitCallback, initCB, 0);
    XtOverrideTranslations(glw, transTable);

    XtRealizeWidget(toplevel);
    installColormap(toplevel,glw);

    XtAppMainLoop(app_context);

}

/* Callbacks */
static void
exposeCB(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    caddr_t call_data;
{
    GLXwinset(XtDisplay(w), XtWindow(w));
    viewcube('t');
}
    
static void
resizeCB(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    GlxDrawCallbackStruct *call_data;
{
    GLXwinset(XtDisplay(w), XtWindow(w));
    viewport(0, (Screencoord) call_data->width-1,
	     0, (Screencoord) call_data->height-1);
    viewcube('t');
}
   
static void
initCB(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    caddr_t call_data;
{
    GLXwinset(XtDisplay(w), XtWindow(w));
    CopyGlColormap(w, colorInfo, XtNumber(colorInfo));
    initialize_gl();
}
    
/* Work proc and related routines */
static Boolean
workProc()
{
    /* verify that we haven't cleaned up somehow */
    if (!workProcInfo.workProcId)
	XtError ("workProc after cleanup");
    (*workProcInfo.doit)();
    return (FALSE);	/* call this work proc again */
}

static void
doRotate()
{
    *workProcInfo.un.ap += workProcInfo.parm;
    viewcube(workProcInfo.axis);
}

static void
rotateCleanup()
{
    updatemat(workProcInfo.axis);     /* incorporate this rotation into */
    *workProcInfo.un.ap = 0;            /* cumulative rotation matrix */
}

static void
doTranslate()
{
    *workProcInfo.un.cp += workProcInfo.parm;
    viewcube('t');
}
     
/* Called when the button is released, or when any other button is pressed,
 * cancel the work proc, call any cleanup routine, and clear the
 * workProcInfo structure.
 */
static void
cleanup()
{
    if (!workProcInfo.workProcId)
	return;
    XtRemoveWorkProc(workProcInfo.workProcId);
    if (workProcInfo.cleanup)
	(*workProcInfo.cleanup)();
    bzero(&workProcInfo, sizeof(workProcInfo));
}

/* Action procedures */
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
doneAction(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    cleanup();
}

static void
resetAction(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    int i, j;

    GLXwinset(XtDisplay(w), XtWindow(w));
    cleanup();
    x  =  -50.0;           
    y  =  -50.0;                 
    z  = -400.0;            
    rx = 0;                      
    ry = 0; 
    rz = 0;

    for(i=0;i<4;i++) {
	for(j=0;j<4;j++)
	    cm[i][j] = ident[i][j];
    }
    viewcube('t');
}

static void
translateAction(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    register Coord *cp;
    
    if (*num_params != 2)
	XtWarning ("Insufficient parameters to translate translation");
    else
    {
	cleanup();
	switch (*params[0])
	{
	case 'x':
	    workProcInfo.un.cp = &x;
	    break;
	case 'y':
	    workProcInfo.un.cp = &y;
	    break;
	case 'z':
	    workProcInfo.un.cp = &z;
	    break;
	default:
	    XtWarning ("Unknown axis for translate translation");
	    return;
	}
        workProcInfo.axis = *params[0];
        workProcInfo.parm = atof(params[1]);
        workProcInfo.doit = doTranslate;
        workProcInfo.cleanup = NULL;
        workProcInfo.workProcId = XtAppAddWorkProc(app_context,workProc,0);
    }
}

static void
rotateAction(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
    register Angle *ap;
    
    if (*num_params != 2)
	XtWarning ("Insufficient parameters to rotate translation");
    else
    {
	cleanup();
	switch (*params[0])
	{
	case 'x':
	    workProcInfo.un.ap = &rx;
	    break;
	case 'y':
	    workProcInfo.un.ap = &ry;
	    break;
	case 'z':
	    workProcInfo.un.ap = &rz;
	    break;
	default:
	    XtWarning ("Unknown axis for rotate translation");
	    return;
	}
        workProcInfo.axis = *params[0];
        workProcInfo.parm = atof(params[1]);
        workProcInfo.doit = doRotate;
        workProcInfo.cleanup = rotateCleanup;
        workProcInfo.workProcId = XtAppAddWorkProc(app_context,workProc,0);
    }
}

/* GL initialization; extracted from the original main() */
static void
initialize_gl() {

    short data;
    float scrnaspect;            /* aspect ratio value */
    long xscrnsize;              /* size of screen in x used to set globals  */


    xscrnsize = getgdesc(GD_XPMAX);          /* get/set screen size[/aspect] */
    if (xscrnsize == 1280) {
        keepaspect(5, 4);
	scrnaspect = 1.25;
    } else if (xscrnsize = 1023) {
        keepaspect(4, 3);
	scrnaspect = 1.34;
    } else {
	fprintf(stderr, "Something's EXTREMELY wrong:  ");
	fprintf(stderr, "xscrnsize=%d\n", xscrnsize);
        exit(-1) ;
    }

/*  initialize and set the display to double buffer mode */

    doublebuffer();
    writemask((1<<getplanes())-1);

#ifdef BACKFACE    /* compile with "-DBACKFACE" if you desire to use GL's */
    backface(TRUE);
#endif

    perspective(470,scrnaspect,1.0,10000.0); 

/*  initialize the modeling transformation values          */
    rx =     0;  ry =     0;  rz =      0;
     x = -50.0;   y = -50.0;   z = -400.0;

}

/* The remaining code is lifted verbatem from the original GL program */
viewcube(axis) 
char axis;
{

/*  transform the cube in world space and (if BACKFACE not defined,
    in software, ) check each face for back face elimination
*/

    color(background);
    clear();
    gflush();

    pushmatrix();
    translate(x,y,z);
    pushmatrix();
    translate(50.0,50.0,-50.0);

/* apply rotation about a single axis */
    switch(axis) {
        case('x'): 
            rotate(rx,'x');
            break;
        case('y'): 
            rotate(ry,'y');
            break;
        case('z'): 
            rotate(rz,'z');
            break;
        case('t'): 
            break;
    }

/* apply all prior rotations */
    multmatrix(cm);
    translate(-50.0,-50.0,50.0);

#ifdef BACKFACE    /* compile with "-DBACKFACE" if you desire to use GL's */
    color(red);
    polf(4,pfrnt);
    color(green);
    polf(4,pback);
    color(yellow);
    polf(4,ptop);
    color(blue);
    polf(4,pbot);
    color(magenta);
    polf(4,prsid);
    color(cyan);
    polf(4,plsid);
#else
    color(red);
    if(norm_dot(pfrnt) >= 0.0) polf(4,pfrnt);
    color(green);
    if(norm_dot(pback) >= 0.0) polf(4,pback);
    color(yellow);
    if(norm_dot(ptop) >= 0.0) polf(4,ptop);
    color(blue);
    if(norm_dot(pbot) >= 0.0) polf(4,pbot);
    color(magenta);
    if(norm_dot(prsid) >= 0.0) polf(4,prsid);
    color(cyan);
    if(norm_dot(plsid) >= 0.0) polf(4,plsid);
#endif

    popmatrix(); 
    popmatrix();
    swapbuffers();
    gflush();
}

/*
 *  Function to postmultiply cumulative rotations 
 *     by rotation about a single axis 
 */

updatemat(axis) 
char axis;
{

    pushmatrix();
    loadmatrix(ident);

    switch (axis) {
        case ('x'): 
            rotate(rx,'x');
            break;
        case ('y'): 
            rotate(ry,'y');
            break;
        case ('z'): 
            rotate(rz,'z');
            break;
    }
    
    multmatrix(cm);
    getmatrix(cm);
    popmatrix();

}

/*  This function takes as input an array of points in homogeneous coord.
    which make up a surface or plane.  The unit normal of the surface and
    the eyepoint to surface unit vector are computed and the dot product
    is calculated. norm_dot returns the dot product floating point value
    and the transformed points for the surface.
*/

float norm_dot(passpoly)
Coord passpoly[][3];
{
    int i;
    float a[3],b[3],c[3],d,abs;
#ifdef 3000
    float fabs();
#endif
    Coord postrans [4][3];


/*  apply the current transformation to the surface points    */
    transform(4,passpoly,postrans);

/*  determine two vectors which lie in the specified plane.  The first three
    points are taken from the surface array.  p1, p2, p3 are ordered by the 
    right-hand rule in the right-hand coordinate system:  i.e. points ordered 
    counter-clockwise when on the positive side of the plane or surface are 
    visible--not backfacing--surfaces.
    a[] gets the xyz coords of row 2
    b[] gets the xyz coords of row 0.
*/
/* 1]  determine two vectors.  note this routine assumes they are not in-line
*/
    for(i = 0; i < 3; i++)
        a[i] = postrans[2][i] - postrans[1][i];

    for(i = 0; i < 3; i++)
        b[i] = postrans[0][i] - postrans[1][i];

/* 2]  find the cross product of the two vectors */

    c[0] = a[1] * b[2] - a[2] * b[1];
    c[1] = a[2] * b[0] - a[0] * b[2];
    c[2] = a[0] * b[1] - a[1] * b[0];


/* 3]  calculate the unit normal vector for the plane or poly using the square
    root of the sum of the squares of x, y, and z to determine length of vec-
    tor, then dividing each axis by that length (x/l, y/l, z/l).
*/
    abs = 0.0;
    
    for (i = 0; i < 3; i++) 
        abs += (c[i]*c[i]);
    d = sqrt(abs);

    if (fabs(d) > 0.000001)    {
        for (i = 0; i < 3; i++)
            a[i] = c[i]/d;

/* 4] calculate the unit vector pointing from the eyepoint to the normal of 
    the plane or poly
*/
        abs = 0.0;

        for (i = 0; i < 3; i++) 
            c[i] = postrans[1][i];
        for (i = 0; i < 3; i++) 
            abs = abs + (c[i]*c[i]);
        d = sqrt(abs); 
    
        if (fabs(d) > 0.000001)    {
            for (i = 0; i < 3; i++)
                b[i] = c[i]/d;

/* 5] return the dot product between the eye vector and the plane normal */
            for (i = 0, d=0.0; i < 3; i++)
                d = d + a[i]*b[i];
        }
        else 
             printf("\n\n Magnitude of surface vector is zero!");
    }
    else 
        printf("\n\n Magnitude of eye vector is zero!");
    return(d);
}


/* the transform function.  this demonstrates what would be the equivalent of
   using feedback with xfpt() on the 2000/3000 series IRIS machines: 
   i.e.  simply multiply each vertex point with the current transformtion
   matrix without any clipping, scaling, etc. to derive transformed world
   coordinate values.
*/

transform(n,passpoly,postrans)
long n;
Coord passpoly[][3], postrans[][3];
{
    Matrix ctm;

    pushmatrix();
    getmatrix(ctm);

    postrans[0][0] = passpoly[0][0]*ctm[0][0] + 
                     passpoly[0][1]*ctm[1][0] +
                     passpoly[0][2]*ctm[2][0] + ctm[3][0];
    postrans[0][1] = passpoly[0][0]*ctm[0][1] + 
                     passpoly[0][1]*ctm[1][1] +
                     passpoly[0][2]*ctm[2][1] + ctm[3][1];
    postrans[0][2] = passpoly[0][0]*ctm[0][2] +
                     passpoly[0][1]*ctm[1][2] +
                     passpoly[0][2]*ctm[2][2] + ctm[3][2];
          
    postrans[1][0] = passpoly[1][0]*ctm[0][0] + 
                     passpoly[1][1]*ctm[1][0] +
                     passpoly[1][2]*ctm[2][0] + ctm[3][0];
    postrans[1][1] = passpoly[1][0]*ctm[0][1] + 
                     passpoly[1][1]*ctm[1][1] +
                     passpoly[1][2]*ctm[2][1] + ctm[3][1];
    postrans[1][2] = passpoly[1][0]*ctm[0][2] +
                     passpoly[1][1]*ctm[1][2] +
                     passpoly[1][2]*ctm[2][2] + ctm[3][2];
                     
    postrans[2][0] = passpoly[2][0]*ctm[0][0] + 
                     passpoly[2][1]*ctm[1][0] +
                     passpoly[2][2]*ctm[2][0] + ctm[3][0];
    postrans[2][1] = passpoly[2][0]*ctm[0][1] + 
                     passpoly[2][1]*ctm[1][1] +
                     passpoly[2][2]*ctm[2][1] + ctm[3][1];
    postrans[2][2] = passpoly[2][0]*ctm[0][2] +
                     passpoly[2][1]*ctm[1][2] +
                     passpoly[2][2]*ctm[2][2] + ctm[3][2];
                     
    postrans[3][0] = passpoly[3][0]*ctm[0][0] + 
                     passpoly[3][1]*ctm[1][0] +
                     passpoly[3][2]*ctm[2][0] + ctm[3][0];
    postrans[3][1] = passpoly[3][0]*ctm[0][1] + 
                     passpoly[3][1]*ctm[1][1] +
                     passpoly[3][2]*ctm[2][1] + ctm[3][1];
    postrans[3][2] = passpoly[3][0]*ctm[0][2] +
                     passpoly[3][1]*ctm[1][2] +
                     passpoly[3][2]*ctm[2][2] + ctm[3][2];
                     
    popmatrix();
}
