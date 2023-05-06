/*
 *                               mscrn_rotate_scale.c
 *
 *     This version of mscrn_rotate uses a Motif control panel including
 *  scales to control the motions.  The reset capability has been removed
 *  from this version to keep the code cleaner.  To implement it, the
 *  scales would need to be globally visible so XtSetValues could be
 *  done on them to reset them to their original values.
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
#include <Xm/MainW.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <Xm/Frame.h>
#include <Xm/Scale.h>
#include <Xm/Label.h>
#include <Xm/Form.h>
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
Angle orx, ory, orz;
float norm_dot();

String fallback_resources[] = {
    "*frame*shadowType: SHADOW_IN",
    "*glwidget*width: 600",
    "*glwidget*height: 600",
    "*XmScale*orientation: HORIZONTAL",
    "*XmScale*scaleWidth: 250",
    "*controlPanel*numColumns: 2",
    "*controlPanel.orientation: HORIZONTAL",
    "*controlPanel.spacing: 20",
    "*controlPanel.XmRowColumn.isAligned: TRUE",
    "*controlPanel.XmRowColumn.entryAlignment: ALIGNMENT_CENTER",
    "*controlPanel.translate.label.labelString: translate",
    "*controlPanel.rotate.label.labelString: rotate",
    "*controlPanel.XmRowColumn.x.titleString: x",
    "*controlPanel.XmRowColumn.y.titleString: y",
    "*controlPanel.XmRowColumn.z.titleString: z",
    "*controlPanel.translate.x.value: -50",
    "*controlPanel.translate.x.minimum: -450",
    "*controlPanel.translate.x.maximum: 350",
    "*controlPanel.translate.x.scaleMultiple: 250",
    "*controlPanel.translate.y.value: -50",
    "*controlPanel.translate.y.minimum: -450",
    "*controlPanel.translate.y.maximum: 350",
    "*controlPanel.translate.z.value: -400",
    "*controlPanel.translate.z.minimum: -2000",
    "*controlPanel.translate.z.maximum: -50",
    "*controlPanel.rotate.XmScale.value: 0",
    "*controlPanel.rotate.XmScale.minimum: -3600",
    "*controlPanel.rotate.XmScale.maximum: 3600",
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
static void quitCB();
static void translateCB();
static void rotateCB();
/* forward declaration of workproc */
static Boolean workProc();
/* other forward declarations */
static void initialize_gl();
static Widget createMenuBar();
static Widget createControlPanel();


/* The GLX configuration parameter:
 * 	Double buffering
 *	color index (default so unspecified)
 *	nothing else special
 */
static GLXconfig glxConfig [] = {
    { GLX_NORMAL, GLX_DOUBLE, TRUE },
    { 0, 0, 0 }
};

main(argc, argv)
int argc;
char *argv[];
{
    Arg args[20];
    int n = 0;
    Widget toplevel, mainw, menu_bar, control, frame, glw;

    toplevel = XtAppInitialize(&app_context, "4DgiftsGlx", 
			       (XrmOptionDescList)NULL , 0,
			       (Cardinal*)&argc, 
			       (String*)argv, 
			       fallback_resources,
			       (ArgList)NULL, 0);

    n = 0;
    mainw = XtCreateManagedWidget("mainw",
				  xmMainWindowWidgetClass,
				  toplevel, args, n);
    menu_bar = createMenuBar(mainw);
    control = createControlPanel(mainw);

    n = 0;
    frame = XtCreateManagedWidget("frame",
				  xmFrameWidgetClass,
				  mainw, args, n);
    n = 0;
    XtSetArg(args[n], GlxNglxConfig, glxConfig); n++;
    glw = XtCreateManagedWidget("glwidget",
				  glxMDrawWidgetClass,
				  frame, args, n);
    XtAddCallback(glw, GlxNexposeCallback, exposeCB, 0);
    XtAddCallback(glw, GlxNresizeCallback, resizeCB, 0);
    XtAddCallback(glw, GlxNginitCallback, initCB, 0);

    XmMainWindowSetAreas( mainw, menu_bar, control, NULL, NULL, frame);
    XtRealizeWidget(toplevel);
    installColormap(toplevel,glw);

    XtAppMainLoop(app_context);

}

/* create the menu bar */
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

    return (menu_bar);
}

/*
 * Create an individual scale
 */
static
createOneScale(parent,axis,CB)
Widget parent;
char axis;
XtCallbackProc CB;
{
    Widget scale;
    Arg args[10];
    register int n;
    char name[2];

    name[0] = axis;
    name[1] = '\0';

    n = 0;
    scale = XmCreateScale (parent, name, args, n);
    XtAddCallback (scale, XmNvalueChangedCallback, CB, (caddr_t)axis);
    XtAddCallback (scale, XmNdragCallback, CB, (caddr_t)axis);
    XtManageChild (scale);
}

static createControls(parent,name,CB)
Widget parent;
char *name;
XtCallbackProc CB;
{
    Widget rowcol;
    Widget label;
    Arg args[10];
    register int n;

    n = 0;
    rowcol = XmCreateRowColumn(parent, name, args, n);
    XtManageChild (rowcol);
    n = 0;
    label = XmCreateLabel(rowcol, "label", args, n);
    XtManageChild (label);
    createOneScale (rowcol,'x',CB);
    createOneScale (rowcol,'y',CB);
    createOneScale (rowcol,'z',CB);
}

static Widget
createControlPanel(parent)
Widget parent;
{
    Widget controlPanel;
    Arg args[10];
    register int n;

    n = 0;
    controlPanel = XmCreateRowColumn (parent, "controlPanel", args, n);
    XtManageChild (controlPanel);
    createControls (controlPanel,"translate",translateCB);
    createControls (controlPanel,"rotate",rotateCB);
    return (controlPanel);
}

/* Miscellaneous Callbacks */
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
    
static void
quitCB(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    caddr_t call_data;
{
    exit (0);
}

static void
translateCB(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    XmScaleCallbackStruct *call_data;
{
    char axis = (char)client_data;
    register Coord *cp;

    switch (axis)
    {
    case 'x':
        cp = &x;
        break;
    case 'y':
        cp = &y;
        break;
    case 'z':
        cp = &z;
        break;
    default:
        XtWarning ("Unknown axis for translate translation");
        return;
    }
    *cp = call_data->value;
    viewcube('t');
}
    
static void
rotateCB(w, client_data, call_data)
    Widget w;
    caddr_t client_data;
    XmScaleCallbackStruct *call_data;
{
    char axis = (char)client_data;
    register Angle *ap, *oap;

    switch (axis)
    {
    case 'x':
        ap = &rx;
        oap = &orx;
        break;
    case 'y':
        ap = &ry;
        oap = &ory;
        break;
    case 'z':
        ap = &rz;
        oap = &orz;
        break;
    default:
        XtWarning ("Unknown axis for translate translation");
        return;
    }
    *ap += (call_data->value - *oap);
    *oap = call_data->value;
    viewcube(axis);
    updatemat(axis);
    *ap = 0;
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
    orx =    0;  ory =    0;  orz =     0;
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
