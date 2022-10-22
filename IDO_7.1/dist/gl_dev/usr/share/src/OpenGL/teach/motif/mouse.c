/*
 * mouse.c - double buffered RGBA motif program using mouse motion events
 */
/* compile: cc -o mouse mouse.c -lGLw -lGLU -lGL -lXm -lXt -lX11 */
#include <stdlib.h>
#include <stdio.h>
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include <X11/GLw/GLwMDrawA.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include <GL/glx.h>
#include <GL/glu.h>

static void draw_scene(void);
static void initialize(void);
static void update_view(int mstate, int ox, int oy, int nx, int ny);

static int attribs[] = { GLX_RGBA, GLX_RED_SIZE, 1, GLX_DOUBLEBUFFER, None};

static String fallbackResources[] = {
    "*sgiMode: True",
    "*useSchemes: all",
    "*glxwidget.width: 300", "*glxwidget.height: 300",
    "*frame.shadowType: SHADOW_IN",
    NULL};

static struct {			/* global UI variables - keep them together */
    XtAppContext appctx;
    Widget glxwidget;
    Boolean direct;
    GLfloat trans[3];		/* current translation */
    GLfloat rot[2];		/* current rotation */
} state;

static void
input(Widget w, XtPointer client_data, XtPointer call) {
   char buf[31];
   KeySym keysym;
   XEvent *event = ((GLwDrawingAreaCallbackStruct *) call)->event;
   static mstate, omx, omy, mx, my;

   switch(event->type) {
   case KeyRelease:
      XLookupString(&event->xkey, buf, sizeof buf, &keysym, NULL);
      switch(keysym) {
      case XK_Escape:
	 exit(EXIT_SUCCESS);
	 break;
      default: break;
      }
      break;
    case ButtonPress:
	if (event->xbutton.button == Button2) {
	    mstate |= 2;
	    mx = event->xbutton.x;
	    my = event->xbutton.y;
	} else if (event->xbutton.button == Button1) {
	    mstate |= 1;
	    mx = event->xbutton.x;
	    my = event->xbutton.y;
	}
	break;
    case ButtonRelease:
	if (event->xbutton.button == Button2)
	    mstate &= ~2;
	else if (event->xbutton.button == Button1)
	    mstate &= ~1;
	break;
    case MotionNotify:
	if (mstate) {
	    omx = mx;
	    omy = my;
	    mx = event->xbutton.x;
	    my = event->xbutton.y;
	    update_view(mstate, omx,mx,omy,my);
	}
	break;
   }
}

static void
resize(Widget w, XtPointer client_data, XtPointer call) {
   GLwDrawingAreaCallbackStruct *call_data;
   call_data = (GLwDrawingAreaCallbackStruct *) call;

   glViewport(0, 0, call_data->width, call_data->height);
}

static void
expose(Widget w, XtPointer client_data, XtPointer call) {
    GLwDrawingAreaCallbackStruct *call_data;
    call_data = (GLwDrawingAreaCallbackStruct *) call;

    draw_scene();
}

main(int argc, char *argv[]) {
    Display        *dpy;
    XVisualInfo    *visinfo;
    GLXContext      glxcontext;
    Widget          toplevel, frame;

    toplevel = XtOpenApplication(&state.appctx, "mouse", NULL, 0, &argc, argv,
		fallbackResources, applicationShellWidgetClass, NULL, 0);
    dpy = XtDisplay(toplevel);

    frame = XmCreateFrame(toplevel, "frame", NULL, 0);
    XtManageChild(frame);

    /* specify visual directly */
    if (!(visinfo = glXChooseVisual(dpy, DefaultScreen(dpy), attribs)))
	XtAppError(state.appctx, "no suitable RGB visual");

    state.glxwidget = XtVaCreateManagedWidget("glxwidget", glwMDrawingAreaWidgetClass,
	frame, GLwNvisualInfo, visinfo, NULL);
    XtAddCallback(state.glxwidget, GLwNexposeCallback, expose, NULL);
    XtAddCallback(state.glxwidget, GLwNresizeCallback, resize, NULL);
    XtAddCallback(state.glxwidget, GLwNinputCallback, input, NULL);

    XtRealizeWidget(toplevel);

    glxcontext = glXCreateContext(dpy, visinfo, 0, GL_TRUE);
    state.direct = glXIsDirect(dpy, glxcontext);
    GLwDrawingAreaMakeCurrent(state.glxwidget, glxcontext);

    initialize();

    XtAppMainLoop(state.appctx);
}

static void
initialize(void) {
    glShadeModel(GL_FLAT);
    gluPerspective(40., 3.0/2.0, 0.001, 100000.0);
    glTranslatef(0.0, 0.0, -3.0);
    glClearColor(0.2,0.2,0.2,0.);
}

static void
side(void) { /* make a square translated 0.5 in the z direction */
    glPushMatrix();
    glTranslatef(0.0,0.0,0.5);
    glRectf(-0.5,-0.5,0.5,0.5);
    glPopMatrix();
}

static void
cube(void) { /* make a cube out of 4 squares */
    glPushMatrix();
    side();
    glRotatef(90.,1.,0.,0.);
    side();
    glRotatef(90.,1.,0.,0.);
    side();
    glRotatef(90.,1.,0.,0.);
    side();
    glPopMatrix();
}

static void
draw_scene() {

    glClear(GL_COLOR_BUFFER_BIT);
    glPushMatrix();
    glTranslatef(state.trans[0], state.trans[1], state.trans[2]);
    glRotatef(state.rot[0], 1.0, 0.0, 0.0);
    glRotatef(state.rot[1], 0.0, 1.0, 0.0);
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    glColor3f(.1, .1, .8);
    cube();
    glScalef(0.3,0.3,0.3);
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    glColor3f(.8, .8, .1);
    cube();
    glPopMatrix();
    GLwDrawingAreaSwapBuffers(state.glxwidget);
    if (!state.direct) glFinish();	/* hack to improve net interactivity */
}

static void
update_view(int mstate, int ox, int nx, int oy, int ny) {
    int dx = ox - nx;
    int dy = ny - oy;

    switch(mstate) {
    case 1:	/* pan */
	state.trans[0] -= dx/100.;
	state.trans[1] -= dy/100.;
	break;
    case 2:	/* rotate */
	state.rot[0] += (dy * 180.0)/500.;
	state.rot[1] -= (dx * 180.0)/500.;
#define clamp(x) x = x > 360. ? x-360. : x < -360. ? x += 360. : x 
	clamp(state.rot[0]);
	clamp(state.rot[1]);
	break;
    case 3:	/* zoom */
	state.trans[2] -= (dx+dy)/100.;
	break;
    }
    draw_scene();
}
