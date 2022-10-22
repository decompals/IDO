/*
 ***********************************************************************
 * ogl_jello.c - OpenGL version
 * Jun Yu - August, '95	rewrite from IrisGL version jello.c 
 *	    by Thant Tessman August, '87
 ***********************************************************************
 */

#include <stdio.h>
#include <math.h>
#include <device.h>
#include <Xm/MainW.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/CascadeB.h>
#include <Xm/Frame.h>
#include <X11/GLw/GLwMDrawA.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glx.h>

#define	TRUE		1
#define FALSE		0

#define X 		0
#define Y 		1
#define Z 		2
#define W 		3

#define RAD 		1.8

#define HEIGHT 		3.0
#define ANG 		42.861
#define PI 		3.1415926536
#define NEAR_ZERO	0.001

/* 32x32 pattern */
GLushort halftone[] = {
    0x5555, 0x5555, 0xaaaa, 0xaaaa,
    0x5555, 0x5555, 0xaaaa, 0xaaaa,
    0x5555, 0x5555, 0xaaaa, 0xaaaa,
    0x5555, 0x5555, 0xaaaa, 0xaaaa,
    0x5555, 0x5555, 0xaaaa, 0xaaaa,
    0x5555, 0x5555, 0xaaaa, 0xaaaa,
    0x5555, 0x5555, 0xaaaa, 0xaaaa,
    0x5555, 0x5555, 0xaaaa, 0xaaaa,
    0x5555, 0x5555, 0xaaaa, 0xaaaa,
    0x5555, 0x5555, 0xaaaa, 0xaaaa,
    0x5555, 0x5555, 0xaaaa, 0xaaaa,
    0x5555, 0x5555, 0xaaaa, 0xaaaa,
    0x5555, 0x5555, 0xaaaa, 0xaaaa,
    0x5555, 0x5555, 0xaaaa, 0xaaaa,
    0x5555, 0x5555, 0xaaaa, 0xaaaa,
    0x5555, 0x5555, 0xaaaa, 0xaaaa };

GLint shadow_pattern;

float dot();
float plane_point_d();
float light_vector[3];
void draw_everything(void);
Boolean animate (XtPointer);
void resize (Widget w, XtPointer, XtPointer);
void start_spin (Widget w, XtPointer data, XtPointer callData);
void quit (Widget w, XtPointer data, XtPointer callData);
void toggle_display_mode (Widget w, XtPointer data, XtPointer callData);
void reset_jello(void);
void input (Widget w, XtPointer data, XtPointer callData);
void map_state_changed(Widget w, XtPointer data, XEvent * event, Boolean * cont);
void add_gravity (Widget w, XtPointer data, XtPointer callData);

static int dblBuf[] = {
	GLX_DOUBLEBUFFER, GLX_RGBA, GLX_DEPTH_SIZE, 16,
	GLX_RED_SIZE, 1, GLX_GREEN_SIZE, 1, GLX_BLUE_SIZE, 1,
	None
};

static	int *snglBuf = &dblBuf[1];

static String resources[] = {
	"*sgiMode: true",
	"*useSchemes: all",
	"*title: OpenGL jello demo",
	"*glxarea*width: 300",
	"*glxarea*height: 240", 
	NULL
};

float view[4][4] = { 
		{1.0, 0.0, 0.0, 0.0} ,
		{0.0, 1.0, 0.0, 0.0} ,
		{0.0, 0.0, 1.0, 0.0} ,
		{0.0, 0.0, 0.0, 1.0} };

float color_table[16][3] = { 
		{0.0, 0.0, 0.0}, 
		{1.0, 0.0, 0.0},
		{0.0, 1.0, 0.0},
		{0.0, 0.0, 1.0},
		{1.0, 0.0, 1.0},
		{1.0, 1.0, 0.0},
		{0.0, 1.0, 1.0},
		{1.0, 1.0, 1.0},
		{1.0, 0.5, 0.5}, 
		{0.5, 1.0, 0.5},
		{0.5, 0.5, 1.0},
		{1.0, 1.0, 0.5},
		{1.0, 0.5, 1.0},
		{0.5, 1.0, 1.0},
		{0.6, 0.3, 0.1},
		{0.3, 0.1, 0.6} };

float wall_color[6][3] = { 
		{0.7, 0.3, 0.3},
		{0.3, 0.3, 0.7},
		{0.3, 0.7, 0.3},
		{0.7, 0.7, 0.3},
		{0.7, 0.3, 0.7},
		{0.3, 0.7, 0.7} };

float shadow[6][4][4];

Display	*display;
Widget	glxarea, toplevel;
XtAppContext	app;
GLXContext	cx;
XVisualInfo *	vi;
XtWorkProcId	workId = 0;

short omx, omy, nmx, nmy;

float dt = 0.5;
float dw = 0.9;
float damp = 0.3;
float fric = 0.3;
float dist = 15.0;

typedef struct atom_struct {

	float acc[3];

	float vel[3];

	float pos[3];

	float norm[3];

	int center;

	float colur;

	struct atom_struct *next;

	struct atom_struct *surf;

} Jatom;


typedef struct conec_struct {

	float r;

	Jatom *from;

	Jatom *tu;

	struct conec_struct *next;

} Conec;


typedef struct triangle_struct {

    Jatom *vertex[3];

    float norm[4];

    struct triangle_struct *next;

} Triangle;


typedef struct normal_struct {

	float norm[3];

} Normal;


typedef struct polygon_struct {

	int n;

	float *polygon;	/* malloced 3 at a time */

	Normal *normals;	/* list of normal structs */

	GLshort *c;

	struct polygon_struct *next;

} Poly;


typedef struct thing_struct {

	Poly *polygons;

	int (*move)();

	struct thing_struct *sub;	/* sub things */

	struct thing_struct *next;	/* next (not affected by matrix) */

} Thing;


Conec *new_conec(next, a1, a2)
Conec *next;
Jatom *a1, *a2;
{

	Conec *conec;


	conec = (Conec *)malloc(sizeof(Conec));

	conec->from = a1;
	conec->tu = a2;

	conec->next = next;

	return(conec);

}


Jatom *new_atom(next)
Jatom *next;
{
    Jatom *atom;

    atom = (Jatom *)malloc(sizeof(Jatom));

    atom->pos[X] = 0.0;
    atom->pos[Y] = 0.0;
    atom->pos[Z] = 0.0;

    atom->vel[X] = 0.0;
    atom->vel[Y] = 0.0;
    atom->vel[Z] = 0.0;

    atom->acc[X] = 0.0;
    atom->acc[Y] = 0.0;
    atom->acc[Z] = 0.0;

    atom->norm[X] = 0.0;
    atom->norm[Y] = 0.0;
    atom->norm[Z] = 0.0;

    atom->center = FALSE;

    atom->surf = NULL;

    atom->next = next;

    return(atom);
}


Triangle *new_triangle(next, a1, a2, a3)
Triangle *next;
Jatom *a1, *a2, *a3;
{
    Triangle *new;

    new = (Triangle *)malloc(sizeof(Triangle));

    new->next = next;

    new->vertex[0] = a1;
    new->vertex[1] = a2;
    new->vertex[2] = a3;
}

Poly *new_polygon() {

	Poly *new;

	new = (Poly *)malloc(sizeof(Poly));

	new->n = 0;
	new->polygon = NULL;
	new->next = NULL;

	return(new);
}


Conec *tmp_conec;
Jatom *tmp_atom;


Jatom *jello_atoms = NULL;
Jatom *surface_atoms = NULL;
Triangle *surface_triangles;
Conec *jello_conec = NULL;
float grav = 0.0;
float grav_vec[3] = {0.0, 0.0, 0.0};

int function=0;
#define REORIENT 		1
#define SPIN 			2 

int display_mode=4;
#define DOTS 			1
#define CONECS 			2
#define SURFACE_DOTS 		3
#define SURFACE_TRIANGLES 	4
#define GOURAUD_SURFACE 	5

main (argc, argv)
int	argc;
char	*argv[];
{
	Widget	menupane, menubar, mainw, btn, frame, cascade;
	Arg     args[10];
	toplevel = XtAppInitialize (&app, "Jello", NULL, 0, &argc, argv,
				resources, NULL, 0);

	display = XtDisplay (toplevel);
	/* find an OpenGL-capable RGB visual with depth buffer */
	vi = glXChooseVisual(display, DefaultScreen(display), dblBuf);
	if (vi == NULL) {
	    vi = glXChooseVisual (display, DefaultScreen(display), snglBuf);
	    if (vi == NULL);
		XtAppError (app, "no RGB visual with depth buffer");
	}
	/* create an OpenGL rendering context */
	cx = glXCreateContext ( display, 
				vi, 
				None,		/* no display list sharing */
				GL_TRUE);	/* favor direct */
	if (cx == NULL) 
	    XtAppError(app, "could not create rendering context");
	XtAddEventHandler(toplevel, StructureNotifyMask, False,
			  map_state_changed, NULL);

	mainw = XmCreateMainWindow(toplevel, "mainw", NULL, 0);
	XtManageChild(mainw);
	
	/* create menu bar */
	menubar = XmCreateMenuBar(mainw, "menubar", NULL, 0);
	XtManageChild(menubar);

	/* first pulldown menu */
	menupane = XmCreatePulldownMenu(menubar, "menupane", NULL, 0);
	XtSetArg(args[0], XmNmnemonic, 'R');
	btn = XmCreatePushButton(menupane, "Reset", args, 1);
	XtAddCallback(btn, XmNactivateCallback, (XtCallbackProc) reset_jello, NULL);
	XtManageChild(btn);
	XtSetArg(args[0], XmNmnemonic, 'S');
	btn = XmCreateToggleButton(menupane, "Spin", args, 1);
	XtAddCallback(btn, XmNvalueChangedCallback, start_spin, NULL);
	XtManageChild(btn);
	XtSetArg(args[0], XmNmnemonic, 'G');
	btn = XmCreateToggleButton(menupane, "Gravity", args, 1);
	XtAddCallback(btn, XmNvalueChangedCallback, add_gravity, NULL);
	XtManageChild(btn);
	XtSetArg(args[0], XmNmnemonic, 'E');
	btn = XmCreatePushButton(menupane, "Exit", args, 1);
	XtAddCallback(btn, XmNactivateCallback, quit, NULL);
	XtManageChild(btn);
	XtSetArg(args[0], XmNsubMenuId, menupane);
	XtSetArg(args[1], XmNmnemonic, 'J');
	cascade = XmCreateCascadeButton(menubar, "Jello", args, 2);
	XtManageChild(cascade);

	/* the second pulldown menu */
	XtSetArg(args[0], XmNradioBehavior, True);
	XtSetArg(args[1], XmNradioAlwaysOne, True);
	XtSetArg(args[2], XmNbuttonSet, display_mode - 1);
	menupane = XmCreatePulldownMenu(menubar, "menupane", args, 3);
	XtSetArg(args[0], XmNmnemonic, 'D');
	btn = XmCreateToggleButton(menupane, "Dots", args, 1);
	XtAddCallback(btn, XmNvalueChangedCallback, toggle_display_mode, 
						(caddr_t) DOTS);
	XtManageChild (btn);
	XtSetArg(args[0], XmNmnemonic, 'C');
	btn = XmCreateToggleButton(menupane, "Conecs", args, 1);
	XtAddCallback(btn, XmNvalueChangedCallback, toggle_display_mode, 
						(caddr_t) CONECS);
	XtManageChild (btn);
	XtSetArg(args[0], XmNmnemonic, 'S');
	btn = XmCreateToggleButton(menupane, "Surface Dots", args, 1);
	XtAddCallback(btn, XmNvalueChangedCallback, toggle_display_mode, 
						(caddr_t) SURFACE_DOTS);
	XtManageChild (btn);
	XtSetArg(args[0], XmNmnemonic, 'T');
	XtSetArg(args[1], XmNset, True);
	btn = XmCreateToggleButton(menupane, "Surface Triangle", args, 2);
	XtAddCallback(btn, XmNvalueChangedCallback, toggle_display_mode, 
						(caddr_t) SURFACE_TRIANGLES);
	XtManageChild (btn);
	XtSetArg(args[0], XmNmnemonic, 'G');
	btn = XmCreateToggleButton(menupane, "Gouraud", args, 1);
	XtAddCallback(btn, XmNvalueChangedCallback, toggle_display_mode, 
						(caddr_t) GOURAUD_SURFACE);
	XtManageChild (btn);
	XtSetArg(args[0], XmNsubMenuId, menupane);
	XtSetArg(args[1], XmNmnemonic, 'D');
	cascade = XmCreateCascadeButton(menubar, "Display", args, 2);
	XtManageChild(cascade);

	/* create framed drawing area for OpenGL rendering */
	frame = XmCreateFrame(mainw, "frame", NULL, 0);
	XtManageChild(frame);

	glxarea = XtVaCreateManagedWidget("glxarea", 
					  glwMDrawingAreaWidgetClass,
					  frame, 
					  GLwNvisualInfo, 
					  vi, 
					  NULL);
	XtAddCallback(glxarea, XmNexposeCallback, 
				(XtCallbackProc)draw_everything, NULL);
	XtAddCallback(glxarea, XmNresizeCallback, resize, NULL);
	XtAddCallback(glxarea, XmNinputCallback, input, NULL);
	/* set up application's window layout */
	XmMainWindowSetAreas(mainw, menubar, NULL, NULL, NULL, frame);
	XtRealizeWidget(toplevel);
	/*
	 * Once widget is realized (ie, associated with a created X window), we
	 * can bind the OpenGL rendering context to the window.
	 */
	glXMakeCurrent(display, XtWindow(glxarea), cx);

	reset_jello();
	initialize();

	/* set up the idle task */
	workId = XtAppAddWorkProc (app, animate, NULL);

	XtAppMainLoop(app);
}


initialize()
{

    float sx, sy, sz;
    int i, j, k;
    GLint mm; 

    glGetIntegerv(GL_INDEX_BITS, &mm);

#ifdef EIGHTPLANE
    if (mm < 8)
#else
    if (mm < 12) 
#endif	    
    {

	fprintf(stderr, "You need at least 12 bitplanes to run %s\n");
	exit(1);
    }

    glShadeModel(GL_SMOOTH);
    glCullFace(GL_FRONT); 

    glEnable(GL_DEPTH_TEST);
    glDepthRange(0.0, 1.0);

    /* set up projection matrix */
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(.1*(400),  5.0/4.0,  dist-(HEIGHT*1.74),  dist+(HEIGHT*1.74));

    /* set up model view matrix */
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glTranslatef(0.0,  0.0,  -dist);

    /* clear color buffer */
    glClearColor(0, 0, 0, 0); 
    glClearDepth(1.); 

    /* make all shadow matrix identity matrix */
    for (k=0; k<6; k++)
	for (i=0; i<4; i++)
	    for (j=0; j<4; j++)
		shadow[k][i][j] = (i==j);

    build_icosahedron();

    /* set gravity */
    grav = 0; 
    light_vector[X] = view[0][1];
    light_vector[Y] = view[1][1];
    light_vector[Z] = view[2][1];

    grav_vec[X] = light_vector[X] * grav;
    grav_vec[Y] = light_vector[Y] * grav;
    grav_vec[Z] = light_vector[Z] * grav;

    /* Initialize shadow stipple */
    shadow_pattern = glGenLists(1);
    glNewList(shadow_pattern, GL_COMPILE);
    glPolygonStipple((const GLubyte *)halftone);
    glEndList();
}


build_icosahedron() {

    int i, j;

    Jatom *a[13];
    Conec *conec;
    float x, y, z, xy, ang=0.0;

    a[0] = new_atom(NULL);

    for (i=1; i<13; i++) {

	a[i] = new_atom(a[i-1]);
    }

    jello_atoms = a[12];

    conec = new_conec(NULL, a[0], a[2]);
    conec = new_conec(conec, a[0], a[3]);
    conec = new_conec(conec, a[0], a[4]);
    conec = new_conec(conec, a[0], a[5]);
    conec = new_conec(conec, a[0], a[6]);

    conec = new_conec(conec, a[1], a[7]);
    conec = new_conec(conec, a[1], a[8]);
    conec = new_conec(conec, a[1], a[9]);
    conec = new_conec(conec, a[1], a[10]);
    conec = new_conec(conec, a[1], a[11]);

    conec = new_conec(conec, a[2], a[3]);
    conec = new_conec(conec, a[2], a[6]);
    conec = new_conec(conec, a[2], a[9]);
    conec = new_conec(conec, a[2], a[10]);

    conec = new_conec(conec, a[3], a[4]);
    conec = new_conec(conec, a[3], a[10]);
    conec = new_conec(conec, a[3], a[11]);

    conec = new_conec(conec, a[4], a[5]);
    conec = new_conec(conec, a[4], a[7]);
    conec = new_conec(conec, a[4], a[11]);

    conec = new_conec(conec, a[5], a[6]);
    conec = new_conec(conec, a[5], a[7]);
    conec = new_conec(conec, a[5], a[8]);

    conec = new_conec(conec, a[6], a[8]);
    conec = new_conec(conec, a[6], a[9]);

    conec = new_conec(conec, a[7], a[8]);
    conec = new_conec(conec, a[7], a[11]);

    conec = new_conec(conec, a[8], a[9]);

    conec = new_conec(conec, a[9], a[10]);

    conec = new_conec(conec, a[10], a[11]);

    conec = new_conec(conec, a[0], a[12]);
    conec = new_conec(conec, a[1], a[12]);
    conec = new_conec(conec, a[2], a[12]);
    conec = new_conec(conec, a[3], a[12]);
    conec = new_conec(conec, a[4], a[12]);
    conec = new_conec(conec, a[5], a[12]);
    conec = new_conec(conec, a[6], a[12]);
    conec = new_conec(conec, a[7], a[12]);
    conec = new_conec(conec, a[8], a[12]);
    conec = new_conec(conec, a[9], a[12]);
    conec = new_conec(conec, a[10], a[12]);
    conec = new_conec(conec, a[11], a[12]);

    jello_conec = conec;

    a[0]->pos[X] = 0.0;
    a[0]->pos[Y] = 0.0;
    a[0]->pos[Z] = -1.0;

    a[1]->pos[X] = 0.0;
    a[1]->pos[Y] = 0.0;
    a[1]->pos[Z] = 1.0;

    z = cos(ANG);
    xy = sin(ANG);

    a[2]->pos[Z] = -z;
    a[3]->pos[Z] = -z;
    a[4]->pos[Z] = -z;
    a[5]->pos[Z] = -z;
    a[6]->pos[Z] = -z;

    a[7]->pos[Z] = z;
    a[8]->pos[Z] = z;
    a[9]->pos[Z] = z;
    a[10]->pos[Z] = z;
    a[11]->pos[Z] = z;

    ang=0.0;
    for (i=2; i<7; i++) {
	x = xy * cos(ang); y = xy * sin(ang);
	a[i]->pos[X] = x; a[i]->pos[Y] = y;
	a[i+5]->pos[X] = -x; a[i+5]->pos[Y] = -y;
	ang += 2.0 * PI / 5.0;
    }

    find_surface();
}



iterate() {

	do_clear(jello_atoms);

	do_accel(jello_conec);

	do_bounds(jello_atoms);

	do_vel(jello_atoms);

	do_pos(jello_atoms);
}


do_clear(atom)
Jatom *atom;
{
	if (atom) {

	    do {
		atom->acc[X] = grav_vec[X];
		atom->acc[Y] = grav_vec[Y];
		atom->acc[Z] = grav_vec[Z];
	    } while (atom = atom->next);
	}
}


do_accel(conec)
Conec *conec;
{
	int i=0;
	float dx, dy, dz, dr, ax, ay, az;

	if (conec) do {

		dx = conec->from->pos[X] - conec->tu->pos[X];
		dy = conec->from->pos[Y] - conec->tu->pos[Y];
		dz = conec->from->pos[Z] - conec->tu->pos[Z];

		dr = sqrt(dx*dx + dy*dy + dz*dz);
		if (conec->from->center) {
		    conec->tu->norm[X] = -dx/dr;
		    conec->tu->norm[Y] = -dy/dr;
		    conec->tu->norm[Z] = -dz/dr;
		    conec->tu->colur = dot(conec->tu->norm,
		    light_vector);
		} else if (conec->tu->center) {
		    conec->from->norm[X] = dx/dr;
		    conec->from->norm[Y] = dy/dr;
		    conec->from->norm[Z] = dz/dr;
		    conec->from->colur = dot(conec->from->norm,
		    light_vector);
		}

		ax = dt * (dx - dx/dr);
		ay = dt * (dy - dy/dr);
		az = dt * (dz - dz/dr);

		conec->from->acc[Y] -= ay;
		conec->from->acc[X] -= ax;
		conec->from->acc[Z] -= az;

		conec->tu->acc[X] += ax;
		conec->tu->acc[Y] += ay;
		conec->tu->acc[Z] += az;

	} while (conec = conec->next);
}


do_bounds(atom)
Jatom *atom;
{
    if (atom) do {

	if (atom->pos[Y] < -(HEIGHT-0.1)) {
	    atom->acc[Y] -= (atom->pos[Y] + (HEIGHT-0.1)) * dw;
	    atom->vel[X] *= fric;
	    atom->vel[Z] *= fric;
	}

	if (atom->pos[Y] > (HEIGHT-0.1)) {
	    atom->acc[Y] -= (atom->pos[Y] - (HEIGHT-0.1)) * dw;
	    atom->vel[X] *= fric;
	    atom->vel[Z] *= fric;
	}

	if (atom->pos[X] < -(HEIGHT-0.1)) {
	    atom->acc[X] -= (atom->pos[X] + (HEIGHT-0.1)) * dw;
	    atom->vel[Y] *= fric;
	    atom->vel[Z] *= fric;
	}

	if (atom->pos[X] > (HEIGHT-0.1)) {
	    atom->acc[X] -= (atom->pos[X] - (HEIGHT-0.1)) * dw;
	    atom->vel[Y] *= fric;
	    atom->vel[Z] *= fric;
	}

	if (atom->pos[Z] < -(HEIGHT-0.1)) {
	    atom->acc[Z] -= (atom->pos[Z] + (HEIGHT-0.1)) * dw;
	    atom->vel[X] *= fric;
	    atom->vel[Y] *= fric;
	}

	if (atom->pos[Z] > (HEIGHT-0.1)) {
	    atom->acc[Z] -= (atom->pos[Z] - (HEIGHT-0.1)) * dw;
	    atom->vel[X] *= fric;
	    atom->vel[Y] *= fric;
	}

    } while (atom = atom->next);
}


do_vel(atom)
Jatom *atom;
{
	if (atom) {

	    do {
		atom->vel[X] += atom->acc[X];
		atom->vel[Y] += atom->acc[Y];
		atom->vel[Z] += atom->acc[Z];

		atom->vel[X] *= damp;
		atom->vel[Y] *= damp;
		atom->vel[Z] *= damp;


	    } while (atom = atom->next);
	}
}


do_pos(atom)
Jatom *atom;
{
	if (atom) {

	    do {
		atom->pos[X] += atom->vel[X];
		atom->pos[Y] += atom->vel[Y];
		atom->pos[Z] += atom->vel[Z];

	    } while (atom = atom->next);
	}
}

void input (Widget w, XtPointer data, XtPointer callData)
{
    XmDrawingAreaCallbackStruct *cd = (XmDrawingAreaCallbackStruct *) callData;
    if (cd->reason == XmCR_INPUT) {
	if (cd->event->type == ButtonPress) {
	    function |= REORIENT;
	    omx = -1;
	    omy = -1;
	    nmx = cd->event->xbutton.x;
	    nmy = cd->event->xbutton.y;
	}
	else if (cd->event->type == ButtonRelease) {
	    function &= (~REORIENT);
	}
	else if (cd->event->type == MotionNotify) {
	    if (function & REORIENT) {
		nmx = cd->event->xmotion.x;
		nmy = cd->event->xmotion.y;
	    }
	} 
    }
}

void toggle_display_mode (Widget w, XtPointer data, XtPointer callData)
{
    if (XmToggleButtonGetState(w)) {
    	display_mode = (int) data;
    }
}

void map_state_changed(Widget w, XtPointer data, XEvent * event, Boolean * cont)
{
    switch (event->type) {
    case MapNotify:
        if (workId != 0) 
		workId = XtAppAddWorkProc(app, animate, NULL);
        break;
    case UnmapNotify:
	XtRemoveWorkProc(workId);
        break;
    }
}

void resize (Widget w, XtPointer data, XtPointer callData)
{
    Dimension	width, height;
    XtVaGetValues (w, XmNwidth, &width, XmNheight, &height, NULL);
    width = 5.0*height/4;
    XtVaSetValues (toplevel, XmNwidth, width, NULL);
    glViewport(0, 0, (GLint) width, (GLint) height);
}

Boolean animate (XtPointer data)
{
    if (function & REORIENT) {
	reorient();
    }
    else if (function & SPIN) {
	spin();
    }
    draw_everything ();
    iterate();
    return False;		/* leave work proc active */
}

void start_spin (Widget w, XtPointer data, XtPointer callData)
{
    if (XmToggleButtonGetState(w)) 
	function |= SPIN;
    else 
	function &= (~SPIN);
}

void add_gravity (Widget w, XtPointer data, XtPointer callData)
{
    grav = 0;
    if (XmToggleButtonGetState(w)) {
	grav = -0.008;
	damp = 0.995;
	light_vector[X] = view[0][1];
	light_vector[Y] = view[1][1];
	light_vector[Z] = view[2][1];

	grav_vec[X] = light_vector[X] * grav;
	grav_vec[Y] = light_vector[Y] * grav;
	grav_vec[Z] = light_vector[Z] * grav;
    }
}

void quit (Widget w, XtPointer data, XtPointer callData)
{
    exit(0);
}

void draw_everything(void)
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glPushMatrix();
    glMultMatrixf((GLfloat *) view);
    draw_floor();
    draw_jello();
    draw_frame(); 
    glPopMatrix();

    /* swap the buffer */
    glXSwapBuffers(display, XtWindow(glxarea));
}


draw_jello()
{
    switch (display_mode) {

	case DOTS:
	    draw_dots(jello_atoms);
	    break;

	case CONECS:
	    draw_conecs(jello_conec);
	    break;

	case SURFACE_DOTS:
	    draw_surface_dots(surface_atoms);
	    break;

	case SURFACE_TRIANGLES:
	    draw_surface_triangles(surface_triangles);
	    break;

	case GOURAUD_SURFACE:
	    draw_gouraud(surface_triangles);
	    break;
    }
}


reorient()
{
    if (omx != -1 && omy != -1) {
	glPushMatrix();

	glLoadIdentity(); 

	glRotatef(.2*((GLfloat) (nmx-omx)), 0.0, 1.0, 0.0);
	glRotatef(.2*((GLfloat) (nmy-omy)), 1.0, 0.0, 0.0);

	glMultMatrixf((GLfloat *) view);

	glGetFloatv(GL_MODELVIEW_MATRIX, (GLfloat *) view);

	light_vector[X] = view[0][1];
	light_vector[Y] = view[1][1];
	light_vector[Z] = view[2][1];

	grav_vec[X] = light_vector[X] * grav;
	grav_vec[Y] = light_vector[Y] * grav;
	grav_vec[Z] = light_vector[Z] * grav;

	glPopMatrix();
    }

    omx=nmx; omy=nmy;
}


spin()
{

	static int a, b, c;
	float sa, sb, sc; 

	sa = sin (a*PI/1800.0);
	sb = sin (b*PI/1800.0);
	sc = sin (c*PI/1800.0); 

	if ((a+=1)>3600) a -= 3600;
	if ((b+=3)>3600) b -= 3600;
	if ((c+=7)>3600) c -= 3600;

	glPushMatrix();

	glLoadIdentity();

	glRotatef(sa, 0.0, 1.0, 0.0);
	glRotatef(sb, 1.0, 0.0, 0.0);
	glRotatef(sc, 0.0, 0.0, 1.0);

	glMultMatrixf ((GLfloat *)view);

	glGetFloatv(GL_MODELVIEW_MATRIX, (GLfloat *) view);

	light_vector[X] = view[0][1];
	light_vector[Y] = view[1][1];
	light_vector[Z] = view[2][1];

	grav_vec[X] = light_vector[X] * grav;
	grav_vec[Y] = light_vector[Y] * grav;
	grav_vec[Z] = light_vector[Z] * grav;

	glPopMatrix();
}


draw_floor() {

	int i;
	float c;
        float ht = HEIGHT;

	if (view[0][2]<ht/dist) {

#ifdef EIGHTPLANE	    
	    glColor3f(wall_color[0][0],
		      wall_color[0][1],
		      wall_color[0][2]);
#else
    	    if (0.0<(c = view[0][1])) c=0.0;
	    c = c * -100.0 + 55.0;

	    glColor3ub((GLuint)c,  (GLuint)c,  (GLuint)c);	    
#endif	    

	    glBegin(GL_POLYGON);
	    glVertex3f(ht,  ht,  ht);
	    glVertex3f(ht,  -ht,  ht);
	    glVertex3f(ht,  -ht,  -ht);
	    glVertex3f(ht,  ht,  -ht);
	    glEnd();

	    if (view[0][1]<0.0) {

		glPushMatrix();

		shadow[0][0][0] = NEAR_ZERO;
		shadow[0][0][1] = -view[1][1]/view[0][1];
		shadow[0][0][2] = -view[2][1]/view[0][1];

		glTranslatef(HEIGHT,  0.0,  0.0);
		glMultMatrixf((GLfloat *) shadow[0]);
		glTranslatef(-HEIGHT,  0.0,  0.0);

		draw_shadow(surface_triangles);

		glPopMatrix();
	    }

	}

	if (view[0][2]>-ht/dist) {

#ifdef EIGHTPLANE	    
            glColor3f(wall_color[1][0],
                      wall_color[1][1],
                      wall_color[1][2]);
#else
	    if (0.0>(c = view[0][1])) c=0.0;
	    c = c * 100.0 + 55.0;
	    
	    glColor3ub((GLuint)c,  (GLuint)c,  (GLuint)c);
#endif	    

	    glBegin(GL_POLYGON);
	    glVertex3f(-ht,  ht,  ht);
	    glVertex3f(-ht,  ht,  -ht);
	    glVertex3f(-ht,  -ht,  -ht);
	    glVertex3f(-ht,  -ht,  ht);
	    glEnd();

	    if (view[0][1]>0.0) {

		glPushMatrix();

		shadow[1][0][0] = NEAR_ZERO;
		shadow[1][0][1] = -view[1][1]/view[0][1];
		shadow[1][0][2] = -view[2][1]/view[0][1];

		glTranslatef(-HEIGHT,  0.0,  0.0);
		glMultMatrixf((GLfloat *) shadow[1]);
		glTranslatef(HEIGHT,  0.0,  0.0);

		draw_shadow(surface_triangles);

		glPopMatrix();
	    }
	} 

	if (view[2][2]>-ht/dist) {

#ifdef EIGHTPLANE	    
            glColor3f(wall_color[2][0],
                      wall_color[2][1],
                      wall_color[2][2]);
#else
	    if (0.0>(c = view[2][1])) c=0.0;
	    c = c * 100.0 + 55.0;
	    
	    glColor3ub((GLuint)c,  (GLuint)c,  (GLuint)c);
#endif	    

	    glBegin(GL_POLYGON);
	    glVertex3f(ht,  ht,  -ht);
	    glVertex3f(ht,  -ht,  -ht);
	    glVertex3f(-ht,  -ht,  -ht);
	    glVertex3f(-ht,  ht,  -ht);
	    glEnd();

	    if (view[2][1]>0.0) {

		glPushMatrix();

		shadow[2][2][0] = -view[0][1]/view[2][1];
		shadow[2][2][1] = -view[1][1]/view[2][1];
		shadow[2][2][2] = NEAR_ZERO;

		glTranslatef(0.0,  0.0,  -HEIGHT);
		glMultMatrixf((GLfloat *) shadow[2]);
		glTranslatef(0.0,  0.0,  HEIGHT);

		draw_shadow(surface_triangles);

		glPopMatrix();
	    }
	}

	if (view[2][2]<ht/dist) {
	    
#ifdef EIGHTPLANE	    
            glColor3f(wall_color[3][0],
                      wall_color[3][1],
                      wall_color[3][2]);
#else
	    if (0.0<(c = view[2][1])) c=0.0;
	    c = c * -100.0 + 55.0;
	    
	    glColor3ub((GLuint)c,  (GLuint)c,  (GLuint)c);
#endif	    

	    glBegin(GL_POLYGON);
	    glVertex3f(ht,  ht,  ht);
	    glVertex3f(-ht,  ht,  ht);
	    glVertex3f(-ht,  -ht,  ht);
	    glVertex3f(ht,  -ht,  ht);
	    glEnd();

	    if (view[2][1]<0.0) {

		glPushMatrix();

		shadow[3][2][0] = -view[0][1]/view[2][1];
		shadow[3][2][1] = -view[1][1]/view[2][1];
		shadow[3][2][2] = NEAR_ZERO;

		glTranslatef(0.0,  0.0,  HEIGHT);
		glMultMatrixf((GLfloat *) shadow[3]);
		glTranslatef(0.0,  0.0,  -HEIGHT);

		draw_shadow(surface_triangles);

		glPopMatrix();
	    }

	}

	if (view[1][2]<ht/dist) {

#ifdef EIGHTPLANE	    
            glColor3f(wall_color[4][0],
                      wall_color[4][1],
                      wall_color[4][2]);
#else
	    if (0.0<(c = view[1][1])) c=0.0;
	    c = c * -100.0 + 55.0;
	    
	    glColor3ub((GLuint)c,  (GLuint)c,  (GLuint)c);
#endif	    

	    glBegin(GL_POLYGON);
	    glVertex3f(ht,  ht,  ht);
	    glVertex3f(ht,  ht,  -ht);
	    glVertex3f(-ht,  ht,  -ht);
	    glVertex3f(-ht,  ht,  ht);
	    glEnd();

	    if (view[1][1]<0.0) {

		glPushMatrix();

		shadow[4][1][0] = -view[0][1]/view[1][1];
		shadow[4][1][1] = NEAR_ZERO;
		shadow[4][1][2] = -view[2][1]/view[1][1];

		glTranslatef(0.0,  HEIGHT,  0.0);
		glMultMatrixf((GLfloat *) shadow[4]);
		glTranslatef(0.0,  -HEIGHT,  0.0);

		draw_shadow(surface_triangles);

		glPopMatrix();
	    }

	}

	if (view[1][2]>-ht/dist) {

#ifdef EIGHTPLANE	    
            glColor3f(wall_color[5][0],
                      wall_color[5][1],
                      wall_color[5][2]);
#else
	    if (0.0>(c = view[1][1])) c=0.0;
	    c = c * 100.0 + 55.0;
	    
	    glColor3ub((GLuint)c,  (GLuint)c,  (GLuint)c);
#endif	    

	    glBegin(GL_POLYGON);
	    glVertex3f(ht,  -ht,  ht);
	    glVertex3f(-ht,  -ht,  ht);
	    glVertex3f(-ht,  -ht,  -ht);
	    glVertex3f(ht,  -ht,  -ht);
	    glEnd();

	    if (view[1][1]>0.0) {

		glPushMatrix();

		shadow[5][1][0] = -view[0][1]/view[1][1];
		shadow[5][1][1] = NEAR_ZERO;
		shadow[5][1][2] = -view[2][1]/view[1][1];

		glTranslatef(0.0,  -HEIGHT,  0.0);
		glMultMatrixf((GLfloat *) shadow[5]);
		glTranslatef(0.0,  HEIGHT,  0.0);

		draw_shadow(surface_triangles);

		glPopMatrix();
	    }

	}
}

draw_frame()
{
	int i;

#ifdef EIGHTPLANE
	glColor3f(0., 0., 0.);
#else	
	glColor3ub(50,  100,  255);
#endif	


	glBegin(GL_LINE_STRIP); 

	glVertex3f(-HEIGHT+0.02,  -HEIGHT+0.02,  -HEIGHT+0.02);
	glVertex3f(-HEIGHT+0.02,  -HEIGHT+0.02,  HEIGHT-0.02);
	glVertex3f(-HEIGHT+0.02,  HEIGHT-0.02,  HEIGHT-0.02);
	glVertex3f(-HEIGHT+0.02,  HEIGHT-0.02,  -HEIGHT+0.02);
	glVertex3f(-HEIGHT+0.02,  -HEIGHT+0.02,  -HEIGHT+0.02);
	glVertex3f(HEIGHT-0.02,  -HEIGHT+0.02,  -HEIGHT+0.02);
	glVertex3f(HEIGHT-0.02,  -HEIGHT+0.02,  HEIGHT-0.02);
	glVertex3f(HEIGHT-0.02,  HEIGHT-0.02,  HEIGHT-0.02);
	glVertex3f(HEIGHT-0.02,  HEIGHT-0.02,  -HEIGHT+0.02);
	glVertex3f(HEIGHT-0.02,  -HEIGHT+0.02,  -HEIGHT+0.02);
	
	glEnd();

	glBegin(GL_LINE_STRIP); 
	glVertex3f(-HEIGHT+0.02,  -HEIGHT+0.02,  HEIGHT-0.02);
	glVertex3f(HEIGHT-0.02,  -HEIGHT+0.02,  HEIGHT-0.02);
	glEnd();

	glBegin(GL_LINE_STRIP); 
	glVertex3f(-HEIGHT+0.02,  HEIGHT-0.02,  -HEIGHT+0.02);
	glVertex3f(HEIGHT-0.02,  HEIGHT-0.02,  -HEIGHT+0.02);
	glEnd();

	glBegin(GL_LINE_STRIP); 
	glVertex3f(-HEIGHT+0.02,  HEIGHT-0.02,  HEIGHT-0.02);
	glVertex3f(HEIGHT-0.02,  HEIGHT-0.02,  HEIGHT-0.02);
	glEnd();

}


void reset_jello() 
{
    surface_atoms = NULL;
    free_atoms(jello_atoms);
    jello_atoms = NULL;
    free_conecs(jello_conec);
    jello_conec = NULL;
    free_triangles(surface_triangles);
    surface_triangles = NULL;

    grav_vec[X] = 0.0;
    grav_vec[Y] = 0.0;
    grav_vec[Z] = 0.0;

    build_icosahedron();
}


free_atoms(atom)
Jatom *atom;
{
    if (atom) {
	if (atom->next) {
	    free_atoms(atom->next);
	    atom->next=NULL;
	} else {
	    free(atom);
	}
    }
}


free_conecs(conec)
Conec *conec;
{
    if (conec) {
	if (conec->next) {
	    free_conecs(conec->next);
	} else {
	    free(conec);
	}
    }
}


free_triangles(triangle)
Triangle *triangle;
{
    if (triangle) {
	if (triangle->next) {
	    free_triangles(triangle->next);
	} else {
	    free(triangle);
	}
    }
}


find_surface()
{

    int i;
    Jatom *atom, *a1, *a2, *a3, *a4;
    Conec *conec, *c1, *c2, *c3, *c4;
    Triangle *triangle;

    free_triangles(surface_triangles);
    surface_triangles = NULL;
    triangle = NULL;
    surface_atoms = NULL;
    atom = jello_atoms;

    if (atom && jello_conec) do {

	conec = jello_conec;
	i=0;

	do {
	    if (conec->from == atom || conec->tu == atom) i++;
	} while (conec = conec->next);

	if (i<12) {
	    atom->surf = surface_atoms;
	    surface_atoms = atom;
	} else {
	    atom->center = TRUE;
	}

    } while (atom = atom->next);

    a1 = surface_atoms;

    do {

	a2 = a1;
	while (a2 = a2->surf) {

	    a3 = a2;
	    while (a3 = a3->surf) {

		c1 = jello_conec;
		do {

		    if (c1->from==a1 && c1->tu==a2 || 
			c1->tu==a1 && c1->from==a2) {

			c2 = jello_conec;
			do {

			    if (c2->from==a2 && c2->tu==a3 ||
				c2->tu==a2 && c2->from==a3) {

				c3 = jello_conec;
				do {

				    if (c3->from==a3 && c3->tu==a1 ||
					c3->tu==a3 && c3->from==a1) {

					triangle =
					new_triangle(triangle, a1,
					a2, a3);

					face_right(triangle);
				    }
				} while (c3=c3->next);
			    }
			} while (c2=c2->next);
		    }
		} while (c1=c1->next);
	    }
	}

    } while (a1 = a1->surf);

    surface_triangles = triangle;
}


draw_dots(atom)
Jatom *atom;
{
#ifdef EIGHTPLANE
	int	tcolor = 0;
#else	
	glColor3ub(200,  100,  50);
#endif	

	if (atom) do {
#ifdef EIGHTPLANE
	    glColor3f ( color_table[tcolor%16][0],
			color_table[tcolor%16][1],
			color_table[tcolor%16][2]);
	    tcolor ++;
#endif

	    glBegin(GL_POINTS);
	    glVertex3f(atom->pos[X],  atom->pos[Y],  atom->pos[Z]);
	    glEnd();
	} while (atom = atom->next);

}


draw_surface_dots(atom)
Jatom *atom;
{
#ifdef EIGHTPLANE
	int	tcolor = 0;
#else	
	glColor3ub(100,  200,  50);
#endif	

	if (atom) do {
#ifdef EIGHTPLANE
	    glColor3f ( color_table[tcolor%16][0],
			color_table[tcolor%16][1],
			color_table[tcolor%16][2]);
	    tcolor ++;
#endif

	    glBegin(GL_POINTS);
	    glVertex3f(atom->pos[X],  atom->pos[Y],  atom->pos[Z]);
	    glEnd();
	} while (atom = atom->surf);

}


draw_surface_triangles(triangle)
Triangle *triangle;
{
    float norm[3], r, c;
    float v1[3], v2[3];
    float polygon[3][3];
    int	i;

#ifdef EIGHTPLANE    
    int tcolor = 0; 
#endif    

    if (triangle) do {

	polygon[0][X] = triangle->vertex[0]->pos[X];
	polygon[0][Y] = triangle->vertex[0]->pos[Y];
	polygon[0][Z] = triangle->vertex[0]->pos[Z];

	polygon[1][X] = triangle->vertex[1]->pos[X];
	polygon[1][Y] = triangle->vertex[1]->pos[Y];
	polygon[1][Z] = triangle->vertex[1]->pos[Z];

	polygon[2][X] = triangle->vertex[2]->pos[X];
	polygon[2][Y] = triangle->vertex[2]->pos[Y];
	polygon[2][Z] = triangle->vertex[2]->pos[Z];

	v1[X] = polygon[1][X] - polygon[0][X];
	v1[Y] = polygon[1][Y] - polygon[0][Y];
	v1[Z] = polygon[1][Z] - polygon[0][Z];

	v2[X] = polygon[2][X] - polygon[0][X];
	v2[Y] = polygon[2][Y] - polygon[0][Y];
	v2[Z] = polygon[2][Z] - polygon[0][Z];

	cross(v1, v2, norm);

	r = sqrt(norm[X] * norm[X] + 
		 norm[Y] * norm[Y] + 
		 norm[Z] * norm[Z]);

	norm[X] /= r;
	norm[Y] /= r;
	norm[Z] /= r;

#ifdef EIGHTPLANE
	glColor3f (color_table[tcolor%16][0], 
		   color_table[tcolor%16][1],
		   color_table[tcolor%16][2]);	
	tcolor ++;
#else
	if ((c = dot(norm, light_vector))<0.0) c = 0.0;

	glColor3ub ((GLubyte) (c * 156.0 + 100.0), 
		    (GLubyte) (c * 156.0 + 100.0),
		    (GLubyte) (c * 156.0 + 100.0));
#endif
	glBegin(GL_POLYGON); 
	for(i = 0; i < 3; i++) 
	    glVertex3fv((GLfloat *) polygon[i]); 
	glEnd();

    } while (triangle=triangle->next);

}


draw_shadow(triangle)
Triangle *triangle;
{

    glCallList(shadow_pattern);
    glEnable(GL_POLYGON_STIPPLE);

#ifdef EIGHTPLANE
    glColor3f (0.0, 0.0, 0.0);
#else    
    glColor3ub(0,  0,  0);
#endif

    if (triangle) do {

	    glBegin(GL_POLYGON);
	    glVertex3f( triangle->vertex[0]->pos[X],
			triangle->vertex[0]->pos[Y],
			triangle->vertex[0]->pos[Z]);
	    glVertex3f( triangle->vertex[1]->pos[X],
			triangle->vertex[1]->pos[Y],
			triangle->vertex[1]->pos[Z]);
	    glVertex3f( triangle->vertex[2]->pos[X],
			triangle->vertex[2]->pos[Y],
			triangle->vertex[2]->pos[Z]);
	    glEnd();

    } while (triangle=triangle->next) ;
    glDisable(GL_POLYGON_STIPPLE);
}

draw_gouraud(triangle)
Triangle *triangle;
{
    float c;

    if (triangle) do {

	glBegin(GL_POLYGON);

	if ((c = triangle->vertex[0]->colur)<0.0) c = 0.0;

	glColor3ub ((GLubyte)(c * 100.0 + 30.0),
		    (GLubyte)(c * 50.0 + 15.0), 
		    (GLubyte)(c * 200.0 + 50.0));

	glVertex3f(triangle->vertex[0]->pos[X],
		   triangle->vertex[0]->pos[Y],
		   triangle->vertex[0]->pos[Z]);

	if ((c = triangle->vertex[1]->colur)<0.0) c = 0.0;

	glColor3ub ((GLubyte)(c * 100.0 + 30.0),
		    (GLubyte)(c * 50.0 + 15.0), 
		    (GLubyte)(c * 200.0 + 50.0));

	glVertex3f(triangle->vertex[1]->pos[X],
		   triangle->vertex[1]->pos[Y],
		   triangle->vertex[1]->pos[Z]);

	if ((c = triangle->vertex[2]->colur)<0.0) c = 0.0;

	glColor3ub ((GLubyte)(c * 100.0 + 30.0),
		    (GLubyte)(c * 50.0 + 15.0), 
		    (GLubyte)(c * 200.0 + 50.0));

	glVertex3f(triangle->vertex[2]->pos[X],
		   triangle->vertex[2]->pos[Y],
		   triangle->vertex[2]->pos[Z]);

	glEnd();

    } while (triangle=triangle->next);

}


draw_conecs(conec)
Conec *conec;
{

#ifdef EIGHTPLANE
	int	tcolor = 0;
#else
    glColor3ub(100,  255,  200); 
#endif

    if (conec) do {
#ifdef EIGHTPLANE
	glColor3f ( color_table[tcolor%16][0],
		    color_table[tcolor%16][1],
		    color_table[tcolor%16][2]);
	tcolor ++;
#endif

	glBegin(GL_LINE_STRIP);
	glVertex3f(conec->from->pos[X],
		   conec->from->pos[Y],
		   conec->from->pos[Z]);
	glVertex3f(conec->tu->pos[X],
		   conec->tu->pos[Y],
		   conec->tu->pos[Z]);
	glEnd();

    } while (conec = conec->next);
}


cross(v1, v2, norm)
float v1[3], v2[3], norm[3];
{
    norm[X] = v1[Y] * v2[Z] - v2[Y] * v1[Z];
    norm[Y] = v1[Z] * v2[X] - v2[Z] * v1[X];
    norm[Z] = v1[X] * v2[Y] - v2[X] * v1[Y];
}


float dot(v1, v2)
float v1[3], v2[3];
{
    return( v1[X] * v2[X] +
	    v1[Y] * v2[Y] +
	    v1[Z] * v2[Z]);
}


face_right(triangle)
Triangle *triangle;
{

    Jatom *ta;
    float p[3];

    p[X] = 0.0;
    p[Y] = 0.0;
    p[Z] = 0.0;

    if (plane_point_d(triangle, p)>0.0) {
	ta = triangle->vertex[2];
	triangle->vertex[2] = triangle->vertex[1];
	triangle->vertex[1] = ta;
    }
}


float plane_point_d(triangle, atom)
Triangle *triangle;
Jatom *atom;
{
    float v1[3], v2[3], norm[4], d, r;

    v1[X] = triangle->vertex[1]->pos[X]-triangle->vertex[0]->pos[X];
    v1[Y] = triangle->vertex[1]->pos[Y]-triangle->vertex[0]->pos[Y];
    v1[Z] = triangle->vertex[1]->pos[Z]-triangle->vertex[0]->pos[Z];

    v2[X] = triangle->vertex[2]->pos[X]-triangle->vertex[0]->pos[X];
    v2[Y] = triangle->vertex[2]->pos[Y]-triangle->vertex[0]->pos[Y];
    v2[Z] = triangle->vertex[2]->pos[Z]-triangle->vertex[0]->pos[Z];

    cross(v1, v2, norm);

    r = sqrt(norm[X] * norm[X] + 
	     norm[Y] * norm[Y] + 
	     norm[Z] * norm[Z]);

    norm[X] /= r;
    norm[Y] /= r;
    norm[Z] /= r;

    norm[W] = -(triangle->vertex[0]->pos[X] * norm[X] +
		triangle->vertex[0]->pos[Y] * norm[Y] +
		triangle->vertex[0]->pos[Z] * norm[Z]);

    d = norm[X] * atom->pos[X] + 
	norm[Y] * atom->pos[Y] + 
	norm[Z] * atom->pos[Z] + norm[W];

    if (d<0.0) {
	triangle->norm[X] = -norm[X];
	triangle->norm[Y] = -norm[Y];
	triangle->norm[Z] = -norm[Z];
    } else {
	triangle->norm[X] = norm[X];
	triangle->norm[Y] = norm[Y];
	triangle->norm[Z] = norm[Z];
    }

    return(d);
}

